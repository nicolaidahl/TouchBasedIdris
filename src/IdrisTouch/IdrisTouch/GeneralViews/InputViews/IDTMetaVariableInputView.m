//
// Created by Nicolai Dahl on 17/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTMetaVariableInputView.h"

@implementation IDTMetaVariableInputView {

}


- (id)initAndLayout {
    self = [super initAndLayout];
    if (self) {
        self.metavariableButton.rac_command = self.didPressMetaVariable;
    }

    return self;
}


- (void)addSubviews {
    [self addSubview:self.metavariableButton];
}

- (void)defineLayout {
    [self.metavariableButton mas_updateConstraints:^(MASConstraintMaker *make) {
        make.edges.equalTo(self);

    }];
}

- (RACCommand *)didPressMetaVariable {
    if(!_didPressMetaVariable)
    {
        _didPressMetaVariable = [[RACCommand alloc] initWithSignalBlock:^RACSignal *(id input) {
            return [RACSignal return:input];
        }];

    }

    return _didPressMetaVariable;
}

- (UIButton *)metavariableButton {
    if(!_metavariableButton)
    {
        _metavariableButton = [UIButton buttonWithType:UIButtonTypeRoundedRect];
        [_metavariableButton setTitle:@"???" forState:UIControlStateNormal];
        _metavariableButton.titleLabel.font = [UIFont fontWithName:@"Menlo-Bold" size:14];
    }

    return _metavariableButton;
}


@end