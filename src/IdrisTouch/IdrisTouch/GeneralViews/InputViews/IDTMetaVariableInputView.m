//
// Created by Nicolai Dahl on 17/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTMetaVariableInputView.h"

@interface IDTMetaVariableInputView ()

@property (nonatomic, strong) UIButton *metavariableButton;

@end

@implementation IDTMetaVariableInputView {

}


- (void)addSubviews {
    [self addSubview:self.metavariableButton];
}

- (void)defineLayout {
    [self.metavariableButton mas_updateConstraints:^(MASConstraintMaker *make) {
        make.edges.equalTo(self);
        make.width.equalTo(@(50));
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
    }

    return _metavariableButton;
}


@end