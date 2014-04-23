//
// Created by Nicolai Dahl on 19/04/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTGroupInputView.h"
#import "IDTTextFieldInputView.h"


@implementation IDTGroupInputView {

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


- (void) addInputView: (IDTInputView *) inputView
{


    [self addSubview:inputView];
    [self.inputViews addObject:inputView];

    if(self.inputViews.count > 1)
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