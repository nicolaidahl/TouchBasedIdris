//
// Created by Nicolai Dahl on 07/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTAbstractView.h"
#import "IDTAbstractHierarchyView.h"

@class IDTNameTypeGroupInputView;
@class IDTTextFieldGroupInputView;


@interface IDTInferenceRuleView : IDTAbstractView

@property (nonatomic, strong) IDTNameTypeGroupInputView *conclusionInputView;
@property (nonatomic, strong) IDTTextFieldGroupInputView *premisesInputGroup;
@end