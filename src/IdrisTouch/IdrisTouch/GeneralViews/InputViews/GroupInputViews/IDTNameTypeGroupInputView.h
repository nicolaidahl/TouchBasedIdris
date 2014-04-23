//
// Created by Nicolai Dahl on 17/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTTextFieldGroupInputView.h"
#import "IDTTextFieldInputView.h"


@interface IDTNameTypeGroupInputView : IDTTextFieldGroupInputView

@property (nonatomic, readonly) IDTTextFieldInputView *nameInputView;
@property (nonatomic, readonly) IDTTextFieldInputView *typeInputView;

@property (nonatomic, strong) RACSignal *nameTextSignal;
@property (nonatomic, strong) RACSignal *typeTextSignal;

- (id)initAndLayoutNameTypeGroupInputView;
@end