//
// Created by Nicolai Dahl on 14/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTTextFieldGroupInputView.h"
#import "IDTTextFieldInputView.h"


@interface IDTTextFieldGroupInputView ()


@end

@implementation IDTTextFieldGroupInputView {
    MASConstraint *_rightConstraint;
    IDTInputViewBorderStyle _borderStyle;
    IDTGroupInputViewSeparatorType _inputViewSeparatorType;

    RACSubject *_textChangedSignal;

    NSNumber *_exactNumberOfInputViews;

}




- (id)initAndLayoutWithExactNumberOfInputViews:(NSNumber *)exactNumberOfInputViews separatorType:
        (IDTGroupInputViewSeparatorType)separatorType andBoderStyle: (IDTInputViewBorderStyle) borderStyle
{
    self = [super initWithFrame:CGRectZero];

    if (self) {
        _exactNumberOfInputViews = exactNumberOfInputViews;
        _inputViewSeparatorType = separatorType;
        _borderStyle = borderStyle;

        [self runInitialLayoutRoutine];
    }

    return self;
}





- (void)addSubviews {

    if(!_exactNumberOfInputViews)
        [self addInputView];
    else
    {
        for (int j = 0; j < [_exactNumberOfInputViews integerValue]; j++)
            [self addInputView];
    }


}



- (void)defineLayout {

    [self.inputViews enumerateObjectsUsingBlock:^(IDTTextFieldInputView *inputView, NSUInteger idx, BOOL *stop) {
        //Uninstall any right constraint added so far
        [_rightConstraint uninstall];


        if (idx == 0)
            [inputView mas_updateConstraintsWithLeftMarginRelativeToSuperview];
        else {
            IDTTextFieldInputView *leftNeighbor = self.inputViews[idx - 1];
            UIImageView *leftSeparatorNeighbor = self.separatorViews[idx - 1];

            [leftSeparatorNeighbor mas_updateConstraintsWithLeftMarginRelativeTo:leftNeighbor.mas_right];
            [inputView mas_updateConstraintsWithLeftMarginRelativeTo:leftSeparatorNeighbor.mas_right];
        }


        if (idx == self.inputViews.count - 1)
            [inputView mas_updateConstraints:^(MASConstraintMaker *make) {
                _rightConstraint = make.right.equalTo(inputView.superview);
            }];


        [inputView mas_updateConstraintsWithBottomMarginRelativeToSuperview];
        [inputView mas_updateConstraintsWithTopMarginRelativeToSuperview];
    }];

    [self.separatorViews enumerateObjectsUsingBlock:^(UIImageView *separatorView, NSUInteger idx, BOOL *stop) {
        [separatorView mas_updateConstraints:^(MASConstraintMaker *make) {
            make.centerY.equalTo(separatorView.superview);
        }];
    }];

}

- (void)addInputView:(IDTInputView *)inputView {


    NSAssert([inputView isKindOfClass:[IDTTextFieldInputView class]], @"");

    [super addInputView:inputView];

    RACSignal *mergedTextSignal = [self.inputViews.rac_sequence
            foldLeftWithStart:[RACSignal return:nil]
                       reduce:^RACSignal *(RACSignal *accumulator,
                               IDTTextFieldInputView *value) {
                           return [RACSignal merge:@[accumulator, value.textField.rac_textSignal]];
                       }];

    [_textChangedSignal sendNext:mergedTextSignal];

}


- (void) addInputView
{
    IDTTextFieldInputView *iv = [[IDTTextFieldInputView alloc] initAndLayoutWithBorderStyle:_borderStyle];
    [[iv.textField rac_textSignal] subscribeNext:^(NSString *text) {
        if (self.inputViews.count > 0) {
            IDTTextFieldInputView *lastInputView = ((IDTTextFieldInputView *)self.inputViews[self.inputViews.count -
                    1]);
            if (lastInputView.textField == iv.textField) {
                if(![text isEqualToString:@""] && (!_exactNumberOfInputViews || self.inputViews.count <
                            [_exactNumberOfInputViews integerValue]))
                {
                    [self addInputView];
                    [self updateConstraints];
                }
            }
        }
    }];
    iv.cas_styleClass = @"group-input-view";


    [self addInputView: iv];


}





- (RACSignal *)textChangedSignal {
    if(!_textChangedSignal)
    {
        _textChangedSignal = [RACSubject subject];
    }

    return _textChangedSignal;
}

@end