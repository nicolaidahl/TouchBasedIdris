//
// Created by Nicolai Dahl on 17/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTTextFieldInputView.h"


@interface IDTMetaVariableInputView : IDTInputView

@property (nonatomic, strong) RACCommand *didPressMetaVariable;

@property (nonatomic, strong) UIButton *metavariableButton;
@end