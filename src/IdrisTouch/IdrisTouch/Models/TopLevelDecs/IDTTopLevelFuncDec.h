//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTTopLevelDec.h"

@class IDTExpression;


@interface IDTTopLevelFuncDec : IDTTopLevelDec

@property (nonatomic, strong) NSString *identifier;
@property (nonatomic, strong) IDTExpression *type;
@property (nonatomic, strong) NSArray *clauses;

@end