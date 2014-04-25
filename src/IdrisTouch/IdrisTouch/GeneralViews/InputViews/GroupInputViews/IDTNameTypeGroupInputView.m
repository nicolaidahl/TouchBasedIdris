//
// Created by Nicolai Dahl on 17/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTNameTypeGroupInputView.h"
#import "IDTTextFieldInputView.h"
#import "IDTDashedTextField.h"


@implementation IDTNameTypeGroupInputView {

}

- (id)initAndLayoutNameTypeGroupInputView {
    self = [super initAndLayoutWithExactNumberOfInputViews:@2 separatorType:IDTGroupInputViewSeparatorColon
                                             andBoderStyle:IDTInputBorderStyleSolid];
    if (self) {

        self.typeInputView.textField.placeholder = @"Type";
        self.nameInputView.textField.placeholder = @"Identifier";

    }

    return self;
}

- (IDTTextFieldInputView *)typeInputView {
    return self.inputViews[1];;
}

- (IDTTextFieldInputView *)nameInputView {
    return self.inputViews[0];
}

- (RACSignal *)nameTextSignal {
    if(!_nameTextSignal)
    {
        IDTTextFieldInputView *inputView = self.inputViews[0];
        _nameTextSignal = [inputView.textField rac_textSignal];
    }

    return _nameTextSignal;
}

- (RACSignal *)typeTextSignal {
    if(!_typeTextSignal)
    {
        IDTTextFieldInputView *inputView = self.inputViews[1];
        _typeTextSignal = [inputView.textField rac_textSignal];
    }

    return _typeTextSignal;
}


@end