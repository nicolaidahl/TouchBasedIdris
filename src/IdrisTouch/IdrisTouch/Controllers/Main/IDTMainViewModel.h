//
// Created by Nicolai Dahl on 05/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTAbstractViewModel.h"

@class IDTProgram;


@interface IDTMainViewModel : IDTAbstractViewModel

@property (nonatomic, strong) RACCommand *addTopLevelDecCommand;

@property (nonatomic, strong) IDTProgram *program;

@end