//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>

@class IDTProgram;

@protocol IDTJSONSerializable

- (NSDictionary *) dictionaryRepresentation;

@end

@interface IDTJSONSerializer : NSObject
+ (instancetype)serializer;

- (NSData *)serializeObjectHierarchyToData:(id <IDTJSONSerializable>)object;

- (IDTProgram *)deserializeToProgram:(NSDictionary *)dictionary;
@end