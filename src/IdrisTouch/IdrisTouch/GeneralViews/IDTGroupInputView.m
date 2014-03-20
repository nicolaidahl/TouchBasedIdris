//
// Created by Nicolai Dahl on 14/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTGroupInputView.h"
#import "IDTInputView.h"
#import "IDTDashedTextField.h"

@interface IDTGroupInputView ()

@property (nonatomic, strong) NSMutableArray *inputViews;

@end

@implementation IDTGroupInputView {
    MASConstraint *_rightConstraint;
}


- (void)addSubviews {

    [self addInputView];

}

- (void)defineLayout {

    [self.inputViews enumerateObjectsUsingBlock:^(IDTInputView *inputView, NSUInteger idx, BOOL *stop) {
        if (idx == 0)
            [inputView mas_updateConstraintsWithLeftMarginRelativeToSuperview];
        else {
            IDTInputView *leftNeighbor = self.inputViews[idx - 1];
            [inputView mas_updateConstraintsWithLeftMarginRelativeTo:leftNeighbor.mas_right];
        }

        //Uninstall any right constraint added so far
        [_rightConstraint uninstall];

        if (idx == self.inputViews.count - 1)
            [inputView mas_updateConstraints:^(MASConstraintMaker *make) {
                _rightConstraint = make.right.equalTo(inputView.superview);
            }];

        [inputView mas_updateConstraintsWithBottomMarginRelativeToSuperview];
        [inputView mas_updateConstraintsWithTopMarginRelativeToSuperview];


    }];

}


- (void) addInputView
{
    IDTInputView *iv = [[IDTInputView alloc] initAndLayout];
    [[iv.textField rac_textSignal] subscribeNext:^(NSString *text) {
        if (_inputViews.count > 0) {
            IDTInputView *lastInputView = ((IDTInputView*)_inputViews[_inputViews.count - 1]);
            if (lastInputView.textField == iv.textField) {
                if(![text isEqualToString:@""])
                {
                    [self addInputView];
                    [self updateConstraints];
                }
            }
        }
    }];
    iv.cas_styleClass = @"group-input-view";

    [self addSubview:iv];
    [self.inputViews addObject:iv];
}



- (NSMutableArray *)inputViews {
    if(!_inputViews)
    {
        _inputViews = [@[] mutableCopy];
    }

    return _inputViews;
}


@end