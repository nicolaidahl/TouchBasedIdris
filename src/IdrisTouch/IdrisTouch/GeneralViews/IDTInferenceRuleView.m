//
// Created by Nicolai Dahl on 07/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTInferenceRuleView.h"
#import "IDTDashedTextField.h"
#import "IDTTextFieldGroupInputView.h"
#import "IDTTextFieldInputView.h"
#import "IDTNameTypeGroupInputView.h"


@interface IDTInferenceRuleView ()

@property (nonatomic, strong) UIView *separatorLine;


@end

@implementation IDTInferenceRuleView {

}

- (id)initAndLayout {
    self = [super initAndLayout];
    if (self)
    {

    }

    return self;
}


- (void)addSubviews {

    [self addSubview: self.premisesInputGroup];
    [self addSubview: self.separatorLine];
    [self addSubview:self.conclusionInputView];

}

- (void)defineLayout {

    [self.premisesInputGroup mas_updateConstraintsWithTopMarginRelativeToSuperview];
    [self.premisesInputGroup mas_updateConstraints:^(MASConstraintMaker *make) {
        make.width.greaterThanOrEqualTo(@100);
        make.centerX.equalTo(self);
    }];
    [self.premisesInputGroup mas_updateConstraintsHeightFromStylesheet];

    [self.separatorLine mas_updateConstraintsWithTopMarginRelativeTo:self.premisesInputGroup.mas_bottom];
    [self.separatorLine mas_updateConstraints:^(MASConstraintMaker *make) {
        make.left.lessThanOrEqualTo(self.premisesInputGroup);
        make.left.lessThanOrEqualTo(self.conclusionInputView);

        make.right.greaterThanOrEqualTo(self.premisesInputGroup);
        make.right.greaterThanOrEqualTo(self.conclusionInputView);
    }];
    [self.separatorLine mas_updateConstraintsWithRightMarginRelativeToSuperview];
    [self.separatorLine mas_updateConstraintsWithLeftMarginRelativeToSuperview];
    [self.separatorLine mas_updateConstraintsHeightFromStylesheet];
    [self.separatorLine mas_updateConstraintsWithBottomMarginRelativeTo:self.conclusionInputView.mas_top];

    [self.conclusionInputView mas_updateConstraints:^(MASConstraintMaker *make) {
        make.width.greaterThanOrEqualTo(@100);
        make.centerX.equalTo(self);
    }];
    [self.conclusionInputView mas_updateConstraintsHeightFromStylesheet];
    [self.conclusionInputView mas_updateConstraintsWithBottomMarginRelativeToSuperview];

}


- (UIView *)viewThatConnectsThisToViewHierarchy {
    return self.separatorLine;
}



#pragma mark - Accessors


- (UIView *)separatorLine {
    if(!_separatorLine)
    {
        _separatorLine = [UIView new];
        _separatorLine.backgroundColor = [UIColor blackColor];
        _separatorLine.cas_styleClass = @"inference-rule-separator-line";
    }

    return _separatorLine;
}



- (IDTTextFieldGroupInputView *)premisesInputGroup {
    if(!_premisesInputGroup)
    {
        _premisesInputGroup = [[IDTTextFieldGroupInputView alloc] initAndLayoutWithExactNumberOfInputViews:nil
                                                                                        separatorType:IDTGroupInputViewSeparatorArrow
                                                                                        andBoderStyle:IDTInputBorderStyleSolid];
        _premisesInputGroup.cas_styleClass = @"inference-rule-premises-input-group";
    }

    return _premisesInputGroup;
}



- (IDTNameTypeGroupInputView *)conclusionInputView {
    if(!_conclusionInputView)
    {
        _conclusionInputView = [[IDTNameTypeGroupInputView alloc] initAndLayoutNameTypeGroupInputView];
        _conclusionInputView.cas_styleClass = @"inference-rule-conclusion";
    }

    return _conclusionInputView;
}



@end