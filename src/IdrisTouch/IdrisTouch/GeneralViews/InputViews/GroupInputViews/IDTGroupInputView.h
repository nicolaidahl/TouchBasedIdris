//
// Created by Nicolai Dahl on 19/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTInputView.h"

@class IDTTextFieldInputView;

typedef NS_ENUM(NSInteger, IDTGroupInputViewSeparatorType)
{
    IDTGroupInputViewSeparatorSmallSpace = 0,
    IDTGroupInputViewSeparatorLargeSpace,
    IDTGroupInputViewSeparatorArrow,
    IDTGroupInputViewSeparatorColon,
    IDTGroupInputViewSeparatorEqual
};


@interface IDTGroupInputView : IDTInputView

@property(nonatomic, assign) IDTGroupInputViewSeparatorType inputViewSeparatorType;
@property(nonatomic, strong) NSNumber *exactNumberOfInputViews; //If nil -> infinite

@property (nonatomic, strong) NSMutableArray *inputViews;
@property (nonatomic, strong) NSMutableArray *separatorViews;

- (id)initAndLayoutWithExactNumberOfInputViews:(NSNumber *)exactNumberOfInputViews andSeparatorType:
        (IDTGroupInputViewSeparatorType)separatorType;

- (void)addInputView:(IDTInputView *)inputView;

@end