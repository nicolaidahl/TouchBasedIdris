//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>

@class IDTProgram;


@interface IDTAPIClient : NSObject
+ (instancetype)client;

- (RACSignal *)getEvaluationOfObjectHierarchy:(IDTProgram *)program;
@end