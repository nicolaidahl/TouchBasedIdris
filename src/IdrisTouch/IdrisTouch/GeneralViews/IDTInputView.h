//
// Created by Nicolai Dahl on 14/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTAbstractView.h"

@class IDTDashedTextField;

typedef NS_ENUM(NSInteger, IDTInputViewBorderStyle)
{
    IDTInputBorderStyleNone = 0,
    IDTInputBorderStyleSolid,
    IDTInputBorderStyleDashed
};

@interface IDTInputView : IDTAbstractView

@property (nonatomic, strong) UITextField *textField;

@property (nonatomic, assign) IDTInputViewBorderStyle borderStyle;

- (id)initAndLayoutWithBorderStyle:(IDTInputViewBorderStyle)borderStyle;
@end