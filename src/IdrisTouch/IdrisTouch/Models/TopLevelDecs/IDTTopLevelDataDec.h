//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTTopLevelDec.h"
#import "IDTJSONSerializer.h"

@class IDTExpression;


@interface IDTTopLevelDataDec : IDTTopLevelDec <IDTJSONSerializable>

@property (nonatomic, strong) NSString *ident;
@property (nonatomic, strong) IDTExpression *titype;
@property (nonatomic, strong) NSMutableArray *constructors;

@end