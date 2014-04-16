//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTConstant.h"


@interface IDTConstantString : IDTConstant <IDTJSONSerializable>

@property (nonatomic, strong) NSString *string;

@end