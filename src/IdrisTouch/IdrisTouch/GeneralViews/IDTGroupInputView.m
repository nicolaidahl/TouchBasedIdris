//
// Created by Nicolai Dahl on 14/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTGroupInputView.h"
#import "IDTInputView.h"

@interface IDTGroupInputView ()

@property (nonatomic, strong) NSMutableArray *inputViews;

@end

@implementation IDTGroupInputView {

}


- (void)addSubviews {

    for (UITextField *field in self.inputViews) {
        [self addSubview:field];
    }

}

- (void)defineLayout {

    [self.inputViews enumerateObjectsUsingBlock:^(IDTInputView *inputView, NSUInteger idx, BOOL *stop) {
        if (idx == 0)
            [inputView mas_updateConstraintsWithLeftMarginRelativeToSuperview];
        else {
            UITextField *leftNeighbor = self.inputViews[idx - 1];
            [inputView mas_updateConstraintsWithLeftMarginRelativeTo:leftNeighbor.mas_right];
        }

        if (idx == self.inputViews.count - 1)
            [inputView mas_updateConstraintsWithRightMarginRelativeToSuperview];

        [inputView mas_updateConstraintsWithBottomMarginRelativeToSuperview];
        [inputView mas_updateConstraintsWithTopMarginRelativeToSuperview];


    }];

}




- (NSMutableArray *)inputViews {
    if(!_inputViews)
    {
        _inputViews = [@[] mutableCopy];

        for (int j = 0; j < 2; j++) {
            IDTInputView *tf = [[IDTInputView alloc] initAndLayout];
            tf.cas_styleClass = @"group-input-view";

            [_inputViews addObject:tf];

        }
    }

    return _inputViews;
}


@end