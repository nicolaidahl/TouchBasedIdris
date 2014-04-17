//
// Created by Nicolai Dahl on 17/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTGroupInputView.h"


@interface IDTPremissGroupInputView : IDTGroupInputView

@property (nonatomic, strong) RACSignal *premissChangedSignal;

- (id)initPremissGroupInputView;
@end