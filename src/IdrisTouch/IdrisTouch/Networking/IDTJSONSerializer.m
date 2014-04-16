//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTJSONSerializer.h"
#import "CJSONSerializer.h"
#import "IDTProgram.h"
#import "IDTTopLevelDec.h"
#import "DCKeyValueObjectMapping.h"
#import "IDTTopLevelDataDec.h"
#import "IDTTopLevelFuncDec.h"
#import "IDTConstructor.h"
#import "IDTClause.h"


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
            NSArray *array = [((NSArray *) value) mutableCopy];
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
            NSArray *mutableArray = [((NSArray *) value) mutableCopy];
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

    NSString *tag = dictionary[@"tag"];
    if([tag isEqualToString:@"TIDataDec"])
    {
        IDTTopLevelDataDec *dataDec = [[IDTTopLevelDataDec alloc] init];
        dataDec.constructors = [self deserializeConstructors: dictionary[@"constructors"]];

        return dataDec;
    }
    else if([tag isEqualToString:@"TIFunctionDec"])
    {
        IDTTopLevelFuncDec *funcDec = [[IDTTopLevelFuncDec alloc] init];
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
        constructorObject.constructorType = nil;

        [arr addObject:constructorObject];
    }

    return arr;
}

- (NSMutableArray *) deserializeClauses: (NSArray *) clauses {

    NSMutableArray *arr = [@[] mutableCopy];

    for (NSDictionary *clause in clauses) {
        IDTClause *clauseObject = [[IDTClause alloc] init];
        clauseObject.lhs = [@[] mutableCopy];
        clauseObject.rhs = nil;

        [arr addObject:clauseObject];
    }

    return arr;
}

@end