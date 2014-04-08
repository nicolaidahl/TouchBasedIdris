//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTJSONSerializer.h"

@class IDTExpression;


@interface IDTClause : NSObject <IDTJSONSerializable>

@property (nonatomic, strong) NSArray *lhs;
@property (nonatomic, strong) IDTExpression *rhs;

@end