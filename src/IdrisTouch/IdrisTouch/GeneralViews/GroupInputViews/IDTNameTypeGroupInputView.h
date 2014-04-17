//
// Created by Nicolai Dahl on 17/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTGroupInputView.h"
#import "IDTInputView.h"


@interface IDTNameTypeGroupInputView : IDTGroupInputView

@property (nonatomic, readonly) IDTInputView *nameInputView;
@property (nonatomic, readonly) IDTInputView *typeInputView;

@property (nonatomic, strong) RACSignal *nameTextSignal;
@property (nonatomic, strong) RACSignal *typeTextSignal;

- (id)initNameTypeGroupInputView;
@end