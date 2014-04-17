//
// Created by Nicolai Dahl on 05/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTAbstractView.h"

@class IDTInferenceRuleView;



static const int topLevelDecMargin = 20;

@interface IDTMainView : IDTAbstractView

@property (nonatomic, strong) UIButton *addTopLevelDecButton;

- (RACTuple *)addDataDeclaration;

- (RACTuple *)addFunctionDeclaration;
@end