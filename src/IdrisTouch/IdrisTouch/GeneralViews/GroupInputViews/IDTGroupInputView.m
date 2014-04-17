//
// Created by Nicolai Dahl on 14/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTGroupInputView.h"
#import "IDTInputView.h"
#import "IDTDashedTextField.h"



@interface IDTGroupInputView ()

@property (nonatomic, strong) NSMutableArray *separatorViews;

@end

@implementation IDTGroupInputView {
    MASConstraint *_rightConstraint;
}

- (id)initAndLayout {
    self = [super initAndLayout];
    if (self) {
        _inputViewSeparatorType = IDTGroupInputViewSeparatorSmallSpace;
    }

    return self;
}

- (id)initWithExactNumberOfInputViews: (NSNumber *) exactNumberOfInputViews andSeparatorType:
        (IDTGroupInputViewSeparatorType) separatorType {
    self = [super initWithFrame:CGRectZero];
    if (self) {
        _exactNumberOfInputViews = exactNumberOfInputViews;
        _inputViewSeparatorType = separatorType;
    }

    return self;
}


- (id)initAndLayoutWithSeparatorType: (IDTGroupInputViewSeparatorType) separatorType {
    self = [super initAndLayout];
    if (self) {
        _inputViewSeparatorType = separatorType;

    }

    return self;
}


- (void)addSubviews {

    if(!_exactNumberOfInputViews)
        [self addInputView];
    else
    {
        for (int j = 0; j < [_exactNumberOfInputViews integerValue]; j++)
            [self addInputView];
    }


}



- (void)defineLayout {

    [self.inputViews enumerateObjectsUsingBlock:^(IDTInputView *inputView, NSUInteger idx, BOOL *stop) {
        //Uninstall any right constraint added so far
        [_rightConstraint uninstall];


        if (idx == 0)
            [inputView mas_updateConstraintsWithLeftMarginRelativeToSuperview];
        else {
            IDTInputView *leftNeighbor = self.inputViews[idx - 1];
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


- (void) addInputView
{


    IDTInputView *iv = [[IDTInputView alloc] initAndLayout];
    [[iv.textField rac_textSignal] subscribeNext:^(NSString *text) {
        if (_inputViews.count > 0) {
            IDTInputView *lastInputView = ((IDTInputView*)_inputViews[_inputViews.count - 1]);
            if (lastInputView.textField == iv.textField) {
                if(![text isEqualToString:@""] && (!_exactNumberOfInputViews || _inputViews.count <
                            [_exactNumberOfInputViews integerValue]))
                {
                    [self addInputView];
                    [self updateConstraints];
                }
            }
        }
    }];
    iv.cas_styleClass = @"group-input-view";

    [self addSubview:iv];
    [self.inputViews addObject:iv];

    if(_inputViews.count > 1)
    {
        UIView *separatorImageView;

        switch (_inputViewSeparatorType)
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
        }


        separatorImageView.cas_styleClass = @"group-input-view-separator";
        [self addSubview:separatorImageView];
        [self.separatorViews addObject:separatorImageView];

    }



}



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