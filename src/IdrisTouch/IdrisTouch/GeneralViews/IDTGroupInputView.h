//
// Created by Nicolai Dahl on 14/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTAbstractView.h"

typedef NS_ENUM(NSInteger, IDTGroupInputViewSeparatorType)
{
    IDTGroupInputViewSeparatorSmallSpace = 0,
    IDTGroupInputViewSeparatorLargeSpace,
    IDTGroupInputViewSeparatorArrow
};


@interface IDTGroupInputView : IDTAbstractView

@property(nonatomic, readonly) IDTGroupInputViewSeparatorType inputViewSeparatorType;

- (id)initAndLayoutWithSeparatorType:(IDTGroupInputViewSeparatorType)separatorType;
@end