//
// Created by Nicolai Dahl on 17/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTClauseGroupInputView.h"


@interface IDTClauseGroupInputView ()
@property (nonatomic, strong) IDTInputView *lhs;
@property (nonatomic, strong) IDTInputView *rhs;


@end

@implementation IDTClauseGroupInputView {

}

- (id)initAndLayoutWithLhsInputView: (IDTInputView *) lhs andRhsInputView: (IDTInputView *) rhs {
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