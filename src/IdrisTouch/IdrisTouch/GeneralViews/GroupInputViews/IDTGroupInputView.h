//
// Created by Nicolai Dahl on 14/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "IDTAbstractView.h"
#import "IDTInputView.h"

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
@property (nonatomic, assign) IDTInputViewBorderStyle borderStyle;
@property(nonatomic, strong) NSNumber *exactNumberOfInputViews;

@property (nonatomic, strong) NSMutableArray *inputViews;

- (id)initAndLayoutWithExactNumberOfInputViews:(NSNumber *)exactNumberOfInputViews separatorType:
        (IDTGroupInputViewSeparatorType)separatorType andBoderStyle: (IDTInputViewBorderStyle) borderStyle;

- (id)initAndLayoutWithExactNumberOfInputViews:(NSNumber *)exactNumberOfInputViews andSeparatorType:(IDTGroupInputViewSeparatorType)separatorType;

- (void)addInputView:(IDTInputView *)inputView;

- (void)addInputView;
@end