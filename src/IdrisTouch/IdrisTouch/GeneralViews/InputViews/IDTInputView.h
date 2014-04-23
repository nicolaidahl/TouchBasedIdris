//
// Created by Nicolai Dahl on 17/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTAbstractView.h"

typedef NS_ENUM(NSInteger, IDTInputViewBorderStyle)
{
    IDTInputBorderStyleNone = 0,
    IDTInputBorderStyleSolid,
    IDTInputBorderStyleDashed
};

@protocol IDTTextInputView

@property (nonatomic, readonly) RACSignal *textChangedSignal; //Signal of signals

@end


@interface IDTInputView : IDTAbstractView
@end