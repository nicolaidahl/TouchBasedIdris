//
// Created by Nicolai Dahl on 17/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTNameTypeGroupInputView.h"
#import "IDTInputView.h"
#import "IDTDashedTextField.h"


@implementation IDTNameTypeGroupInputView {

}

- (id)initAndLayoutNameTypeGroupInputView {
    self = [super initAndLayoutWithExactNumberOfInputViews:@2 separatorType:IDTGroupInputViewSeparatorColon
                                             andBoderStyle:IDTInputBorderStyleDashed];
    if (self) {



    }

    return self;
}

- (IDTInputView *)typeInputView {
    return self.inputViews[1];;
}

- (IDTInputView *)nameInputView {
    return self.inputViews[0];
}

- (RACSignal *)nameTextSignal {
    if(!_nameTextSignal)
    {
        IDTInputView *inputView = self.inputViews[0];
        _nameTextSignal = [inputView.textField rac_textSignal];
    }

    return _nameTextSignal;
}

- (RACSignal *)typeTextSignal {
    if(!_typeTextSignal)
    {
        IDTInputView *inputView = self.inputViews[1];
        _typeTextSignal = [inputView.textField rac_textSignal];
    }

    return _typeTextSignal;
}


@end