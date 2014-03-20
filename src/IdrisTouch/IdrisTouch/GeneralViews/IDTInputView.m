//
// Created by Nicolai Dahl on 14/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTInputView.h"
#import "IDTDashedTextField.h"

@interface IDTInputView ()



@end

@implementation IDTInputView {

}



- (void)addSubviews {
    [self addSubview: self.textField];
}

- (void)defineLayout {

    [self.textField mas_updateConstraints:^(MASConstraintMaker *make) {
        make.edges.equalTo(self);
    }];

    [self.textField mas_updateConstraintsHeightFromStylesheet];

    [self.textField.rac_textSignal subscribeNext:^(id x) {
        [self updateWidthConstraintForTextField];
    }];


}

- (void) updateWidthConstraintForTextField {
    UIFont *font = [_textField font];

    CGRect textRect = [_textField.text boundingRectWithSize:CGSizeMake(CGFLOAT_MAX, 60)
                                                   options:NSStringDrawingUsesLineFragmentOrigin
                                                attributes:@{NSFontAttributeName:font}
                                                   context:nil];

    CGSize size = CGSizeMake(MAX(textRect.size.width + 14, 40), textRect.size.height);


    [_textField mas_updateConstraints:^(MASConstraintMaker *make) {
        make.width.greaterThanOrEqualTo(@(size.width));
    }];


}

- (IDTDashedTextField *)textField {
    if(!_textField)
    {
        _textField = [IDTDashedTextField new];
        _textField.cas_styleClass = @"input-group-dashed-text-field";
        _textField.autocorrectionType = UITextAutocorrectionTypeNo;
    }

    return _textField;
}


@end