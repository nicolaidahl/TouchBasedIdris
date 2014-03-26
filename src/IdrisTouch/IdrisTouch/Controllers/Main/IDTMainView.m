//
// Created by Nicolai Dahl on 05/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTMainView.h"
#import "IDTInferenceRuleView.h"
#import "IDTDataDeclarationView.h"
#import "IDTFunctionDeclarationView.h"

@interface IDTMainView () <UIToolbarDelegate>

@property (nonatomic, strong) UIToolbar *toolbar;
@property (nonatomic, strong) UIScrollView *scrollView;

@property (nonatomic, strong) NSMutableArray *topLevelDeclarationTuples;
@property (nonatomic, strong) UIImageView *verticalLine;


@end

@implementation IDTMainView {

    NSArray *_topLevelDecConstraints;
}


- (void)addSubviews {
    [self addSubview:self.toolbar];
    [self addSubview:self.scrollView];
    [self.scrollView addSubview:self.verticalLine];
    [self.scrollView addSubview:self.addTopLevelDecButton];

}

- (void) addDataDeclaration
{
    IDTDataDeclarationView *dataDeclarationView = [[IDTDataDeclarationView alloc] initAndLayout];
    dataDeclarationView.cas_styleClass = @"top-level-dec-data";

    UIImageView *lineActionButton = [self newLineActionButton];
    [self.scrollView addSubview:lineActionButton];

    RACTuple *dataDeclarationTuple = RACTuplePack(lineActionButton, dataDeclarationView);

    [self.scrollView insertSubview:dataDeclarationView belowSubview:self.verticalLine];
    [self.topLevelDeclarationTuples addObject:dataDeclarationTuple];
}

- (void) addFunctionDeclaration
{
    IDTFunctionDeclarationView *funcDeclarationView = [[IDTFunctionDeclarationView alloc] initAndLayout];
    funcDeclarationView.cas_styleClass = @"top-level-dec-function";

    UIImageView *lineActionButton = [self newLineActionButton];
    [self.scrollView addSubview:lineActionButton];

    RACTuple *funcDeclarationTuple = RACTuplePack(lineActionButton, funcDeclarationView);

    [self.scrollView insertSubview:funcDeclarationView belowSubview:self.verticalLine];
    [self.topLevelDeclarationTuples addObject:funcDeclarationTuple];
}

- (UIImageView*) newLineActionButton
{
    UIImageView *lineActionButton = [[UIImageView alloc] initWithImage:[UIImage imageNamed:@"line_action_button"]];
    lineActionButton.userInteractionEnabled = YES; //VERY IMPORTANT
    return lineActionButton;
}

- (void) addPanGestureForTopDeclarationTuple: (RACTuple *) topLevelDeclarationTuple
                             withTopNeighbor: (UIView*) topNeighbor
                           andBottomNeighbor: (UIView*) bottomNeighbor
{
    RACTupleUnpack(UIImageView *lineActionButton, IDTAbstractTopLevelDeclarationView *view) = topLevelDeclarationTuple;

    UIPanGestureRecognizer *panGestureRecognizer = [UIPanGestureRecognizer new];
    [lineActionButton addGestureRecognizer:panGestureRecognizer];

    RACSignal *gestureEndedSignal = [panGestureRecognizer.rac_gestureSignal map:^NSNumber *(UIGestureRecognizer
    *gestureRecognizer) {
        return @(gestureRecognizer.state == UIGestureRecognizerStateEnded);
    }];

    RACSignal *gesturePointSignal = [panGestureRecognizer.rac_gestureSignal map:^id(UIPanGestureRecognizer *gesture) {
        return [NSValue valueWithCGPoint:[gesture translationInView:view.superview]];
    }];

    BOOL bottomNeighborIsAddButton = bottomNeighbor == self.addTopLevelDecButton;
    BOOL topNeighborIsToolbar = topNeighbor == self.toolbar;

    [gesturePointSignal subscribeNext:^(NSValue *value) {
        CGPoint point = [value CGPointValue];
        NSLog(@"%f", point.y);

        if(self.topLevelDeclarationTuples.count > 1)
        {

            [view mas_updateConstraints:^(MASConstraintMaker *make) {
                CGPoint p = point;

                p.y = point.y + topLevelDecMargin;

                if(bottomNeighborIsAddButton)
                    p.y = MIN(topLevelDecMargin, point.y + topLevelDecMargin);

                if(topNeighborIsToolbar)
                    p.y = MAX(topLevelDecMargin, point.y + topLevelDecMargin);

                make.top.equalTo(topNeighbor.mas_bottom).offset(p.y);
            }];

            [bottomNeighbor mas_updateConstraints:^(MASConstraintMaker *make) {
                CGPoint p = point;

                p.y = -point.y + topLevelDecMargin;

                if(bottomNeighborIsAddButton)
                    p.y = -MIN(0, point.y);

                if(topNeighborIsToolbar)
                    p.y = -MAX(0, point.y) + topLevelDecMargin;

                make.top.equalTo(view.mas_bottom).offset(p.y);
            }];

        }


    }];

    [gestureEndedSignal subscribeNext:^(NSNumber *ended) {
        if([ended boolValue])
        {
            [view mas_updateConstraints:^(MASConstraintMaker *make) {
                make.top.equalTo(topNeighbor.mas_bottom).with.offset(topLevelDecMargin);
            }];

            [bottomNeighbor mas_updateConstraints:^(MASConstraintMaker *make) {
                if(bottomNeighborIsAddButton)
                    make.top.equalTo(view.mas_bottom);
                else
                    make.top.equalTo(view.mas_bottom).with.offset(topLevelDecMargin);
            }];

            [UIView animateWithDuration:0.3 animations:^{
                [self layoutIfNeeded];
            }];
        }

    }];
}

- (void)defineLayout {

    [self.toolbar mas_updateConstraintsWithLeftMarginRelativeToSuperview];
    [self.toolbar mas_updateConstraintsWithRightMarginRelativeToSuperview];
    [self.toolbar mas_updateConstraintsWithTopMarginRelativeToSuperview];

    [self.scrollView mas_updateConstraintsWithTopMarginRelativeTo:self.toolbar.mas_bottom];
    [self.scrollView mas_updateConstraintsWithLeftMarginRelativeToSuperview];
    [self.scrollView mas_updateConstraintsWithRightMarginRelativeToSuperview];
    [self.scrollView mas_updateConstraintsWithBottomMarginRelativeToSuperview];

    [self.verticalLine mas_updateConstraintsWithLeftMarginRelativeToSuperview];
    [self.verticalLine mas_updateConstraintsWithTopMarginRelativeToSuperview];
    [self.verticalLine mas_updateConstraintsWidthFromStylesheet];

    [self.addTopLevelDecButton mas_updateConstraints:^(MASConstraintMaker *make) {
        make.top.equalTo(self.verticalLine.mas_bottom);
        make.centerX.equalTo(self.verticalLine);
    }];
    [self.addTopLevelDecButton mas_updateConstraintsWithBottomMarginRelativeToSuperview];

    [self.topLevelDeclarationTuples enumerateObjectsUsingBlock:^(RACTuple *tuple, NSUInteger idx, BOOL *stop) {

        RACTupleUnpack(UIImageView *lineActionButton, IDTAbstractTopLevelDeclarationView *topLevelDec) = tuple;

        [lineActionButton.gestureRecognizers enumerateObjectsUsingBlock:^(UIGestureRecognizer *gestureRecognizer,
                NSUInteger idx, BOOL *stop) {
            [lineActionButton removeGestureRecognizer:gestureRecognizer];
        }];

        UIView *viewToConnectTo = [topLevelDec viewThatConnectsThisToViewHierarchy];
        [lineActionButton mas_updateConstraints:^(MASConstraintMaker *make) {
            make.centerX.equalTo(self.verticalLine);
            make.centerY.equalTo(viewToConnectTo);
        }];

        UIView *topNeighbor, *bottomNeighbor;

        if (idx == 0)
            //It has to be the toolbar for the pan to work,
            // as it just makes constraints to mas_bottom of top neighbor no matter what
            topNeighbor = self.toolbar;
        else
            topNeighbor = ((RACTuple*) _topLevelDeclarationTuples[idx - 1]).second;

        if((idx == 0 && _topLevelDeclarationTuples.count == 1) || _topLevelDeclarationTuples.count - 1 == idx)
            bottomNeighbor = self.addTopLevelDecButton;
        else
            bottomNeighbor = ((RACTuple*) _topLevelDeclarationTuples[idx + 1]).second;

        [self addPanGestureForTopDeclarationTuple:tuple withTopNeighbor:topNeighbor
                                andBottomNeighbor:bottomNeighbor];

        [topLevelDec mas_updateConstraints:^(MASConstraintMaker *make) {
            make.top.equalTo(topNeighbor.mas_bottom).with.offset(topLevelDecMargin);
        }];
        [topLevelDec mas_updateConstraintsWithLeftMarginRelativeTo:self.verticalLine.mas_right];
        [topLevelDec mas_updateConstraints:^(MASConstraintMaker *make) {
            make.right.lessThanOrEqualTo(topLevelDec.superview);
        }];


    }];

    //Uninstall old bottom constraints
    [_topLevelDecConstraints enumerateObjectsUsingBlock:^(MASConstraint *constraint, NSUInteger idx, BOOL *stop) {
        [constraint uninstall];
    }];

    //If there is more than zero top level declaration, attach the top of the add button to the bottom of the lowest
    // data declaration. Remember the constraints so that they can be removed when the number of data decs change
    if(self.topLevelDeclarationTuples.count != 0)
    {
        NSMutableArray *constraints = [@[] mutableCopy];

        IDTAbstractTopLevelDeclarationView *lowestConstructor = ((RACTuple*) self
                        .topLevelDeclarationTuples[_topLevelDeclarationTuples.count - 1]).second;
        [constraints addObjectsFromArray:[self.addTopLevelDecButton mas_updateConstraints:^(MASConstraintMaker *make) {
            make.top.equalTo(lowestConstructor.mas_bottom);
        }]];

        _topLevelDecConstraints = constraints;
    }

}



#pragma mark - UIToolbarDelegate

- (UIBarPosition)positionForBar:(id <UIBarPositioning>)bar {
    return UIBarPositionTopAttached;
}


#pragma mark - Accessors

- (UIView *)verticalLine {
    if(!_verticalLine)
    {
        _verticalLine = [[UIView alloc] init];
        _verticalLine.cas_styleClass = @"top-level-dec-vertical-line";
    }
    return _verticalLine;
}

- (UIButton *)addTopLevelDecButton {
    if(!_addTopLevelDecButton)
    {
        _addTopLevelDecButton = [UIButton buttonWithType:UIButtonTypeCustom];
        [_addTopLevelDecButton setImage:[UIImage imageNamed:@"add_button"] forState:UIControlStateNormal];
        _addTopLevelDecButton.cas_styleClass = @"top-level-dec-add-button";
    }

    return _addTopLevelDecButton;
}

- (UIToolbar *)toolbar {
    if(!_toolbar)
    {
        _toolbar = [[UIToolbar alloc] init];
        _toolbar.delegate = self;
        _toolbar.cas_styleClass = @"main-view-toolbar";
    }

    return _toolbar;
}

- (UIScrollView *)scrollView {
    if(!_scrollView)
    {
        _scrollView = [[UIScrollView alloc] init];
        _scrollView.cas_styleClass = @"main-view-scroll-view";
    }

    return _scrollView;
}


- (NSMutableArray *)topLevelDeclarationTuples {
    if(!_topLevelDeclarationTuples)
    {
        _topLevelDeclarationTuples = [@[] mutableCopy];
    }
    return _topLevelDeclarationTuples;
}


@end