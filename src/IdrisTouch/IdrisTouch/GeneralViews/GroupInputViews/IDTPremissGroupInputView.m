//
// Created by Nicolai Dahl on 17/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTPremissGroupInputView.h"
#import "IDTInputView.h"
#import "IDTDashedTextField.h"


@implementation IDTPremissGroupInputView {

}

//Do not rename to initAndLayout. Inifite loop happens
- (id)initAndLayoutPremissGroupInputView {
    self = [super initAndLayoutWithExactNumberOfInputViews:nil separatorType:IDTGroupInputViewSeparatorArrow
                                             andBoderStyle:IDTInputBorderStyleDashed];
    if (self) {

        self.premissChangedSignal = [self.inputViews.rac_sequence
                foldLeftWithStart:[RACSignal return:nil]
                           reduce:^RACSignal *(RACSignal *accumulator,
                                               IDTInputView *value) {
            return [RACSignal merge:@[accumulator, value.textField.rac_textSignal]];
        }];

    }

    return self;
}


@end