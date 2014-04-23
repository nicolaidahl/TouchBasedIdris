//
// Created by Nicolai Dahl on 17/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTTextFieldGroupInputView.h"

@class IDTMetaVariableInputView;


@interface IDTClauseGroupInputView : IDTGroupInputView


@property (nonatomic, strong) IDTMetaVariableInputView *rhs;

@property (nonatomic, strong) IDTTextFieldGroupInputView <IDTTextInputView> *lhs;

- (id)initAndLayoutWithLhsInputView:(IDTTextFieldGroupInputView <IDTTextInputView> *)lhs andRhsInputView:(IDTMetaVariableInputView *)rhs;
@end