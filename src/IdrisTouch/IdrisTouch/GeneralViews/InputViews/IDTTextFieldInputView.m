//
// Created by Nicolai Dahl on 14/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTTextFieldInputView.h"
#import "IDTDashedTextField.h"

@interface IDTTextFieldInputView ()



@end

@implementation IDTTextFieldInputView {

}

- (id)initAndLayoutWithBorderStyle: (IDTInputViewBorderStyle) borderStyle {
    self = [super init];
    if (self) {
        _borderStyle = borderStyle;

        [self runInitialLayoutRoutine];
    }

    return self;
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

    CGSize size = CGSizeMake(MAX(textRect.size.width + 16, 40), textRect.size.height);


    [_textField mas_updateConstraints:^(MASConstraintMaker *make) {
        make.width.greaterThanOrEqualTo(@(size.width));
    }];


}

- (void)setBorderStyle:(IDTInputViewBorderStyle)borderStyle {
    _borderStyle = borderStyle;

    [self.textField removeFromSuperview];
    self.textField = [self textFieldWithBorderStyle:borderStyle];
}


- (UITextField*) textFieldWithBorderStyle: (IDTInputViewBorderStyle) borderStyle
{
    UITextField *textField;

    switch (borderStyle)
    {
        case IDTInputBorderStyleNone:
        {
            textField = [[UITextField alloc] init];
            break;
        }
        case IDTInputBorderStyleDashed:
        {
            textField = [IDTDashedTextField new];
            break;
        }
        case IDTInputBorderStyleSolid:
        {
            textField = [[UITextField alloc] init];
            textField.layer.borderColor = [[UIColor blackColor] CGColor];
            textField.layer.borderWidth = 2;
            textField.layer.cornerRadius = 8.0;

            break;
        }
    }

    textField.cas_styleClass = @"input-group-text-field";
    textField.autocorrectionType = UITextAutocorrectionTypeNo;

    return textField;
}

- (UITextField *)textField {
    if(!_textField)
    {
        _textField = [self textFieldWithBorderStyle:_borderStyle];
    }

    return _textField;
}


@end