//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTExpression.h"


@interface IDTLambda : IDTExpression

@property (nonatomic, strong) NSString *identifier;
@property (nonatomic, strong) IDTExpression *expression;

@end