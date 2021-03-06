//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTConstant.h"


@interface IDTConstantInt : IDTConstant <IDTJSONSerializable>

@property (nonatomic, strong) NSNumber *integer;

- (id)initWithInt:(NSNumber *)integer;
@end