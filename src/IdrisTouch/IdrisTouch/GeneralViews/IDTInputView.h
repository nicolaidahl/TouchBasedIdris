//
// Created by Nicolai Dahl on 14/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTAbstractView.h"

@class IDTDashedTextField;


@interface IDTInputView : IDTAbstractView

@property (nonatomic, strong) IDTDashedTextField *textField;

@end