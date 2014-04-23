//
// Created by Nicolai Dahl on 14/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTAbstractView.h"
#import "IDTInputView.h"

@class IDTDashedTextField;



@interface IDTTextFieldInputView : IDTInputView <IDTTextInputView>

@property (nonatomic, strong) UITextField *textField;

@property (nonatomic, assign) IDTInputViewBorderStyle borderStyle;

- (id)initAndLayoutWithBorderStyle:(IDTInputViewBorderStyle)borderStyle;
@end