//
// Created by Nicolai Dahl on 17/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTClauseGroupInputView.h"
#import "IDTMetaVariableInputView.h"


@implementation IDTClauseGroupInputView {

}

- (id)initAndLayoutWithLhsInputView: (IDTTextFieldGroupInputView <IDTTextInputView> *) lhs andRhsInputView:
        (IDTMetaVariableInputView *) rhs {
    self = [super initWithFrame:CGRectZero];
    if (self) {
        self.exactNumberOfInputViews = @2;
        self.inputViewSeparatorType = IDTGroupInputViewSeparatorEqual;

        self.lhs = lhs;
        self.rhs = rhs;

        [self runInitialLayoutRoutine];
    }

    return self;
}

- (void)addSubviews {

    [self addInputView: self.lhs];
    [self addInputView: self.rhs];

}



@end