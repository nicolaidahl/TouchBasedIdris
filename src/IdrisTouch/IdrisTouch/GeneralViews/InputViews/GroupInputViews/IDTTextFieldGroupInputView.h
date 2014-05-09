//
// Created by Nicolai Dahl on 14/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTAbstractView.h"
#import "IDTAbstractInputView.h"
#import "IDTGroupInputView.h"


@interface IDTTextFieldGroupInputView : IDTGroupInputView <IDTTextInputView>



@property (nonatomic, assign) IDTInputViewBorderStyle borderStyle;

- (id)initAndLayoutWithExactNumberOfInputViews:(NSNumber *)exactNumberOfInputViews separatorType:(IDTGroupInputViewSeparatorType)separatorType andBoderStyle:(IDTInputViewBorderStyle)borderStyle;

- (void)addInputView:(IDTAbstractInputView *)inputView;
- (void)addInputView;

@end