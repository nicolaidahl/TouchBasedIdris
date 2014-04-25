//
// Created by Nicolai Dahl on 14/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTTextFieldInputView.h"
#import "IDTDashedTextField.h"

@interface IDTTextFieldInputView ()


@property (nonatomic, strong) RACCommand *textChangedCommand;
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


    [self.textChangedSignal subscribeNext:^(id x) {
        if(![_textField.text isEqualToString: @""])
            self.borderStyle = IDTInputBorderStyleSolid;

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

    [self setBorderStyle:borderStyle onTextField:self.textField];
}


- (UITextField*) textFieldWithBorderStyle: (IDTInputViewBorderStyle) borderStyle
{
    UITextField *textField = [[UITextField alloc] init];

    [self setBorderStyle:borderStyle onTextField:textField];

    textField.cas_styleClass = @"input-group-text-field";
    textField.autocorrectionType = UITextAutocorrectionTypeNo;

    return textField;
}

- (void) setBorderStyle: (IDTInputViewBorderStyle) borderStyle onTextField: (UITextField *) textField
{
    switch (borderStyle)
    {
        case IDTInputBorderStyleNone:
        {
            textField.layer.borderWidth = 0;
            break;
        }
        case IDTInputBorderStyleSolidGray:
        {
            textField.layer.borderColor = [[UIColor lightGrayColor] CGColor];
            textField.layer.borderWidth = 2;
            textField.layer.cornerRadius = 8.0;

            break;
        }
        case IDTInputBorderStyleSolid:
        {
            textField.layer.borderColor = [[UIColor blackColor] CGColor];
            textField.layer.borderWidth = 2;
            textField.layer.cornerRadius = 8.0;

            break;
        }
    }
}

- (RACSignal *)textChangedSignal {
    return [self.textChangedCommand.executionSignals flatten];
}

- (RACCommand *)textChangedCommand {
    if(!_textChangedCommand)
    {
        _textChangedCommand = [[RACCommand alloc] initWithSignalBlock:^RACSignal *(id input) {
            return [RACSignal return:input];
        }];

        RACSignal *textPropertySignal = RACObserve(self.textField, text);
        RACSignal *mergedSignal = [RACSignal merge:@[textPropertySignal, self.textField.rac_textSignal]];

        [mergedSignal subscribeNext:^(id x) {
            [_textChangedCommand execute:self];
        }];
    }

    return _textChangedCommand;
}


- (UITextField *)textField {
    if(!_textField)
    {
        _textField = [self textFieldWithBorderStyle:_borderStyle];
    }

    return _textField;
}


@end