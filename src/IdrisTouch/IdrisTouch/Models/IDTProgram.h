//
// Created by Nicolai Dahl on 08/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTJSONSerializer.h"


@interface IDTProgram : NSObject <IDTJSONSerializable>

@property (nonatomic, strong) NSString *name;
@@property (nonatomic, strong) NSMutableArray *topLevelDecs;

@end