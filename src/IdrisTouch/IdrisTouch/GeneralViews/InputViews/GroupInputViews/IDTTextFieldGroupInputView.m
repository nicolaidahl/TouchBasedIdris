//
// Created by Nicolai Dahl on 14/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTTextFieldGroupInputView.h"
#import "IDTTextFieldInputView.h"


@interface IDTTextFieldGroupInputView ()


@end

@implementation IDTTextFieldGroupInputView {

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
        self.inputViewSeparatorType = separatorType;
        self.borderStyle = borderStyle;

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