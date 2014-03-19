//
// Created by Nicolai Dahl on 05/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTAbstractView.h"

@class IDTInferenceRuleView;


@interface IDTMainView : IDTAbstractView

@property (nonatomic, strong) UIButton *addTopLevelDecButton;

- (void)addDataDeclaration;

- (void)addFunctionDeclaration;
@end