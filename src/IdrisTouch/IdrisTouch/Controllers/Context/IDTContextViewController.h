//
// Created by Nicolai Dahl on 19/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTAbstractViewController.h"


static NSString *const cellIdentifier = @"context_cell_ident";

@interface IDTContextViewController : IDTAbstractViewController

@property (nonatomic, strong) RACCommand *selectionCommand;

@property (nonatomic, strong) NSArray *options;

- (id)initWithOptions:(NSArray *)options;
@end