//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTJSONSerializer.h"
#import "CJSONSerializer.h"
#import "IDTProgram.h"
#import "IDTTopLevelDec.h"
#import "IDTTopLevelDataDec.h"
#import "IDTTopLevelFuncDec.h"
#import "IDTConstructor.h"
#import "IDTClause.h"
#import "IDTExpression.h"
#import "IDTConstant.h"
#import "IDTConstantExpression.h"
#import "IDTFunctionApplication.h"
#import "IDTLambda.h"
#import "IDTPi.h"
#import "IDTVariable.h"
#import "IDTReference.h"
#import "IDTConstantString.h"
#import "IDTConstantInt.h"
#import "IDTConstantFloat.h"
#import "IDTConstantStringType.h"
#import "IDTConstantIntType.h"
#import "IDTConstantFloatType.h"
#import "IDTConstantTypeType.h"


static NSString *const kContentsName = @"contents";

static NSString *const kTagName = @"tag";

@implementation IDTJSONSerializer {

}

+ (instancetype) serializer {
    return [self new];
}


#pragma mark - Serialize

- (NSData *) serializeObjectHierarchyToData: (id <IDTJSONSerializable>) object
{
    NSDictionary *converted = [self serializeObjectHierarchy:object];
    NSError *error = NULL;

    NSData *jsonData = [[CJSONSerializer serializer] serializeObject:converted error:&error];

    return jsonData;
}

- (NSDictionary *) serializeObjectHierarchy: (id <IDTJSONSerializable>) object
{
    NSMutableDictionary *dictionary = [[object dictionaryRepresentation] mutableCopy];

    return [self serializeDictionary: dictionary];

}

- (NSDictionary *) serializeDictionary: (NSMutableDictionary *) dictionary
{
    for (NSString *key in [dictionary allKeys]) {
        NSObject *value = dictionary[key];

        if ([value isKindOfClass:[NSDictionary class]])
        {
            NSMutableDictionary *mutableDictionary = [((NSDictionary *) value) mutableCopy];
            dictionary[key] = [self serializeDictionary:mutableDictionary];
        }
        else if([value isKindOfClass:[NSArray class]])
        {
            NSMutableArray *array = [((NSArray *) value) mutableCopy];
            dictionary[key] = [self serializeArray:array];
        }
        else if (!([value isKindOfClass:[NSNull class]] || [value isKindOfClass:[NSNumber class]] || [value
                isKindOfClass:[NSString class]] || [value isKindOfClass:[NSData class]]))
        {
            NSAssert([value conformsToProtocol:@protocol(IDTJSONSerializable)],
            @"object types must conform to IDTJSONSerializable protocol to be properly serialized");

            id <IDTJSONSerializable> serializable = (id <IDTJSONSerializable>) value;
            dictionary[key] = [self serializeObjectHierarchy:serializable];
        }

    }

    return dictionary;
}

- (NSArray *) serializeArray: (NSMutableArray *) array
{
    [array enumerateObjectsUsingBlock:^(NSObject *value, NSUInteger idx, BOOL *stop) {

        if ([value isKindOfClass:[NSDictionary class]])
        {
            NSMutableDictionary *mutableDictionary = [((NSDictionary *) value) mutableCopy];
            array[idx] = [self serializeDictionary:mutableDictionary];
        }
        else if([value isKindOfClass:[NSArray class]])
        {
            NSMutableArray *mutableArray = [((NSArray *) value) mutableCopy];
            array[idx] = [self serializeArray:mutableArray];
        }
        else if (!([value isKindOfClass:[NSNull class]] || [value isKindOfClass:[NSNumber class]] || [value
                isKindOfClass:[NSString class]] || [value isKindOfClass:[NSData class]]))
        {
            NSAssert([value conformsToProtocol:@protocol(IDTJSONSerializable)],
            @"object types must conform to IDTJSONSerializable protocol to be properly serialized");

            id <IDTJSONSerializable> serializable = (id <IDTJSONSerializable>) value;
            array[idx] = [self serializeObjectHierarchy:serializable];
        }

    }];

    return array;

}


#pragma mark - Deserialize

- (IDTProgram *) deserializeToProgram: (NSDictionary *) dictionary
{
    IDTProgram *program = [[IDTProgram alloc] initWithName:dictionary[@"name"]];

    for (NSDictionary *topLevelDec in dictionary[@"topLevelDec"]) {
        [program.topLevelDec addObject:[self deserializeTopLevelDec:topLevelDec]];
    }

    return program;
}

- (IDTTopLevelDec*) deserializeTopLevelDec:(NSDictionary *)dictionary {

    NSString *tag = dictionary[kTagName];
    if([tag isEqualToString:@"TIDataDec"])
    {
        IDTTopLevelDataDec *dataDec = [[IDTTopLevelDataDec alloc] init];
        dataDec.ident = dictionary[@"ident"];
        dataDec.titype = [self deserializeExpression: dictionary[@"titype"]];
        dataDec.constructors = [self deserializeConstructors: dictionary[@"constructors"]];

        return dataDec;
    }
    else if([tag isEqualToString:@"TIFunctionDec"])
    {
        IDTTopLevelFuncDec *funcDec = [[IDTTopLevelFuncDec alloc] init];
        funcDec.ident = dictionary[@"ident"];
        funcDec.titype = [self deserializeExpression: dictionary[@"titype"]];
        funcDec.clauses = [self deserializeClauses:dictionary[@"clauses"]];
        return funcDec;
    }
    else
        return nil;

}

- (NSMutableArray *)deserializeConstructors:(NSArray *)constructors {

    NSMutableArray *arr = [@[] mutableCopy];

    for (NSDictionary *constructor in constructors) {
        IDTConstructor *constructorObject = [[IDTConstructor alloc] init];
        constructorObject.constructor = constructor[@"constructor"];
        constructorObject.constructorType = [self deserializeExpression:constructor[@"constructorType"]];

        [arr addObject:constructorObject];
    }

    return arr;
}

- (NSMutableArray *) deserializeClauses: (NSArray *) clauses {

    NSMutableArray *arr = [@[] mutableCopy];

    for (NSDictionary *clause in clauses) {
        IDTClause *clauseObject = [[IDTClause alloc] init];
        NSArray *lhsExpressionDictionaries = clause[@"lhs"];
        NSMutableArray *lhsExpressions = [@[] mutableCopy];

        for (NSDictionary *dictionary in lhsExpressionDictionaries) {
            [lhsExpressions addObject:[self deserializeExpression:dictionary]];
        }

        clauseObject.lhs = lhsExpressions;
        clauseObject.rhs = [self deserializeExpression:clause[@"rhs"]];

        [arr addObject:clauseObject];
    }

    return arr;
}

- (IDTExpression *) deserializeExpression: (NSDictionary *)expressionDictionary
{
    if ([expressionDictionary[kTagName] isEqualToString: @"TIConst"])
    {
        return [[IDTConstantExpression alloc] initWithConstant:[self deserializeConstant:expressionDictionary[kContentsName]]];
    }
    else if([expressionDictionary[kTagName] isEqualToString: @"TIApp"])
    {
        IDTFunctionApplication *functionApplication = [[IDTFunctionApplication alloc] init];
        functionApplication.function = [self deserializeExpression:expressionDictionary[kContentsName][0]];

        NSMutableArray *array = [@[] mutableCopy];

        for (int j = 1; j < ((NSArray*) expressionDictionary[kContentsName]).count; j++) {
            [array addObject:[self deserializeExpression:expressionDictionary[kContentsName][j]]];
        }

        functionApplication.arguments = array;
    }
    else if([expressionDictionary[kTagName] isEqualToString: @"TILam"])
    {
        IDTLambda *lambda = [[IDTLambda alloc] init];

        lambda.identifier = expressionDictionary[kContentsName][0];
        lambda.expression = [self deserializeExpression:expressionDictionary[kContentsName][1]];
        
        return lambda;
    }
    else if([expressionDictionary[kTagName] isEqualToString: @"TIPi"])
    {
        IDTPi *pi = [[IDTPi alloc] init];
        pi.identifier = expressionDictionary[kContentsName][0];
        pi.expr1 = [self deserializeExpression:expressionDictionary[kContentsName][1]];
        pi.expr2 = [self deserializeExpression:expressionDictionary[kContentsName][2]];

        return pi;
    }
    else if([expressionDictionary[kTagName] isEqualToString: @"TIVar"])
    {
        return [[IDTVariable alloc] initWithName:expressionDictionary[kContentsName]];
    }
    else if([expressionDictionary[kTagName] isEqualToString: @"TIRef"])
    {
        return [[IDTReference alloc] initWithVarName:expressionDictionary[kContentsName]];
    }

    NSAssert(NO, @"Unknown expressionDictionary type");
    return nil;
}

- (IDTConstant *) deserializeConstant: (NSDictionary *)constantDictionary
{
    if([constantDictionary[kTagName] isEqualToString: @"TIString"])
    {
        return [[IDTConstantString alloc] initWithString:constantDictionary[kContentsName]];
    }
    else if([constantDictionary[kTagName] isEqualToString: @"TIInt"])
    {
        return [[IDTConstantInt alloc] initWithInt:constantDictionary[kContentsName]];
    }
    else if([constantDictionary[kTagName] isEqualToString: @"TIFloat"])
    {
        return [[IDTConstantFloat alloc] initWithFloat:constantDictionary[kContentsName]];
    }
    else if([constantDictionary[kTagName] isEqualToString: @"TIStringTy"])
    {
        return [IDTConstantStringType new];
    }
    else if([constantDictionary[kTagName] isEqualToString: @"TIIntTy"])
    {
        return [IDTConstantIntType new];
    }
    else if([constantDictionary[kTagName] isEqualToString: @"TIFloatTy"])
    {
        return [IDTConstantFloatType new];
    }
    else if([constantDictionary[kTagName] isEqualToString: @"TITypeTy"])
    {
        return [IDTConstantTypeType new];
    }

    NSAssert(NO, @"Unknown constantDictionary type");
    return nil;
}

@end