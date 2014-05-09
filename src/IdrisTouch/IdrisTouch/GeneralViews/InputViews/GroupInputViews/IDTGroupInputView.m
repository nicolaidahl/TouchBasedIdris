//
// Created by Nicolai Dahl on 19/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTGroupInputView.h"
#import "IDTTextFieldInputView.h"

@interface IDTGroupInputView ()



@end


@implementation IDTGroupInputView {
    MASConstraint *_rightConstraint;
}

- (id)initAndLayoutWithExactNumberOfInputViews:(NSNumber *)exactNumberOfInputViews andSeparatorType:
        (IDTGroupInputViewSeparatorType) separatorType {
    self = [super initWithFrame:CGRectZero];
    if (self) {
        _exactNumberOfInputViews = exactNumberOfInputViews;
        _inputViewSeparatorType = separatorType;

        [self runInitialLayoutRoutine];
    }

    return self;
}

- (void)addSubviews {


}

- (void)defineLayout {

    [self.inputViews enumerateObjectsUsingBlock:^(IDTTextFieldInputView *inputView, NSUInteger idx, BOOL *stop) {
        //Uninstall any right constraint added so far
        [_rightConstraint uninstall];


        if (idx == 0)
            [inputView mas_updateConstraintsWithLeftMarginRelativeToSuperview];
        else {
            IDTTextFieldInputView *leftNeighbor = self.inputViews[idx - 1];
            UIImageView *leftSeparatorNeighbor = self.separatorViews[idx - 1];

            [leftSeparatorNeighbor mas_updateConstraintsWithLeftMarginRelativeTo:leftNeighbor.mas_right];
            [inputView mas_updateConstraintsWithLeftMarginRelativeTo:leftSeparatorNeighbor.mas_right];
        }


        if (idx == self.inputViews.count - 1)
            [inputView mas_updateConstraints:^(MASConstraintMaker *make) {
                _rightConstraint = make.right.equalTo(inputView.superview);
            }];


        [inputView mas_updateConstraintsWithBottomMarginRelativeToSuperview];
        [inputView mas_updateConstraintsWithTopMarginRelativeToSuperview];
    }];

    [self.separatorViews enumerateObjectsUsingBlock:^(UIImageView *separatorView, NSUInteger idx, BOOL *stop) {
        [separatorView mas_updateConstraints:^(MASConstraintMaker *make) {
            make.centerY.equalTo(separatorView.superview);
        }];
    }];

}

- (void) addInputView: (IDTAbstractInputView *) inputView
{
    inputView.index = self.inputViews.count;

    [self addSubview:inputView];
    [self.inputViews addObject:inputView];

    if(self.inputViews.count > 1)
    {
        UIView *separatorImageView;

        switch (self.inputViewSeparatorType)
        {
            case IDTGroupInputViewSeparatorSmallSpace:
            {
                separatorImageView = [[UIView alloc] init];
                [separatorImageView mas_updateConstraints:^(MASConstraintMaker *make) {
                    make.width.equalTo(@2);
                }];
                break;
            }
            case IDTGroupInputViewSeparatorLargeSpace:
            {
                separatorImageView = [[UIView alloc] init];
                [separatorImageView mas_updateConstraints:^(MASConstraintMaker *make) {
                    make.width.equalTo(@10);
                }];
                break;
            }
            case IDTGroupInputViewSeparatorArrow:
            {
                separatorImageView = [[UIImageView alloc] initWithImage:[UIImage
                        imageNamed:@"type_arrow"]];
                separatorImageView.alpha = 0.3;
                break;
            }
            case IDTGroupInputViewSeparatorColon:
            {
                UILabel *label = [[UILabel alloc] init];
                label.text = @" :";
                label.cas_styleClass = @"group-input-view-separator-label";
                separatorImageView = label;
                break;
            }
            case IDTGroupInputViewSeparatorEqual:
            {
                UILabel *label = [[UILabel alloc] init];
                label.text = @" =";
                label.cas_styleClass = @"group-input-view-separator-label";
                separatorImageView = label;
                break;
            }
        }


        separatorImageView.cas_styleClass = @"group-input-view-separator";
        [self addSubview:separatorImageView];
        [self.separatorViews addObject:separatorImageView];

    }


}

#pragma mark - Accessors

- (NSMutableArray *)inputViews {
    if(!_inputViews)
    {
        _inputViews = [@[] mutableCopy];
    }

    return _inputViews;
}

- (NSMutableArray *)separatorViews {
    if(!_separatorViews)
    {
        _separatorViews = [@[] mutableCopy];
    }

    return _separatorViews;
}

@end