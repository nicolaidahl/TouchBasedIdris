//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTJSONSerializer.h"
#import "CJSONSerializer.h"


@implementation IDTJSONSerializer {

}

+ (instancetype) serializer {
    return [self new];
}

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

@end