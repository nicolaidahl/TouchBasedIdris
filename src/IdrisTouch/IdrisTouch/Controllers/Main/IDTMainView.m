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
@property (nonatomic, strong) UIView *verticalLine;

@property (nonatomic, strong) MASConstraint *bottomToLowestViewConstraint;
@property (nonatomic, strong) NSMutableArray *topConstraints;

@end

@implementation IDTMainView {



}



- (void)addSubviews {
    [self addSubview:self.toolbar];
    [self addSubview:self.scrollView];
    [self.scrollView addSubview:self.verticalLine];
    [self.scrollView addSubview:self.addTopLevelDecButton];

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
            self.topConstraints[idx] = make.top.equalTo(topNeighbor.mas_bottom).with.offset(topLevelDecMargin);
        }];
        [topLevelDec mas_updateConstraintsWithLeftMarginRelativeTo:self.verticalLine.mas_right];
        [topLevelDec mas_updateConstraints:^(MASConstraintMaker *make) {
            make.right.lessThanOrEqualTo(topLevelDec.superview);
        }];


    }];

    //Uninstall old bottom constraints
    [_bottomToLowestViewConstraint uninstall];


    //If there is more than zero top level declaration, attach the top of the add button to the bottom of the lowest
    // data declaration. Remember the constraints so that they can be removed when the number of data decs change
    if(self.topLevelDeclarationTuples.count != 0)
    {
        IDTAbstractTopLevelDeclarationView *lowestConstructor = ((RACTuple*) self
                        .topLevelDeclarationTuples[_topLevelDeclarationTuples.count - 1]).second;
        [self.addTopLevelDecButton mas_updateConstraints:^(MASConstraintMaker *make) {
            _bottomToLowestViewConstraint = make.top.equalTo(lowestConstructor.mas_bottom);
        }];
    }

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

#pragma mark - Pan gesture related


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


        if(self.topLevelDeclarationTuples.count > 1)
        {
            //p Top
            CGPoint pTop = point;

            pTop.y = point.y + topLevelDecMargin;

            if(bottomNeighborIsAddButton)
                pTop.y = MIN(topLevelDecMargin, point.y + topLevelDecMargin);

            if(topNeighborIsToolbar)
                pTop.y = MAX(topLevelDecMargin, point.y + topLevelDecMargin);

            BOOL shouldSwitchViewUp = pTop.y < -(view.bounds.size.height/2);

            //p Bottom
            CGPoint pBottom = point;

            pBottom.y = -point.y + topLevelDecMargin;

            if(bottomNeighborIsAddButton)
                pBottom.y = -MIN(0, point.y);

            if(topNeighborIsToolbar)
                pBottom.y = -MAX(0, point.y) + topLevelDecMargin;

            BOOL shouldSwitchViewDown = pBottom.y < -(view.bounds.size.height / 2);

            //Update top constraint
            [view mas_updateConstraints:^(MASConstraintMaker *make) {

                if(shouldSwitchViewUp)
                    [self rewireConstraintsForView:view withMoveDirection:IDTHierarchyMoveDirectionUp];
                else if(!shouldSwitchViewDown)
                    make.top.equalTo(topNeighbor.mas_bottom).offset(pTop.y);

            }];

            //Update bottom constraint
            [bottomNeighbor mas_updateConstraints:^(MASConstraintMaker *make) {

                if(shouldSwitchViewDown)
                    [self rewireConstraintsForView:view withMoveDirection:IDTHierarchyMoveDirectionDown];
                else if(!shouldSwitchViewUp)
                    make.top.equalTo(view.mas_bottom).offset(pBottom.y);

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

- (void) rewireConstraintsForView: (UIView*) view withMoveDirection: (IDTHierarchyMoveDirection) moveDirection
{
    NSUInteger viewIndex = [self indexForTopLevelDeclarationView:view];

    //Remove previous constraints
    for (MASConstraint *constraint in self.topConstraints)
        [constraint uninstall];
    self.topConstraints = [@[] mutableCopy];
    [_bottomToLowestViewConstraint uninstall];

    if(moveDirection == IDTHierarchyMoveDirectionUp)
    {
        NSAssert(viewIndex > 0, @"View can't be on the top position when being rewired to move up");


        RACTuple *temp = _topLevelDeclarationTuples[viewIndex];
        _topLevelDeclarationTuples[viewIndex] = _topLevelDeclarationTuples[viewIndex - 1];
        _topLevelDeclarationTuples[viewIndex - 1] = temp;

    }
    else if (moveDirection == IDTHierarchyMoveDirectionDown)
    {
        NSAssert(viewIndex < self.topLevelDeclarationTuples.count-1, @"View can't be on the bottom position when "
                "being rewired to move down");

        RACTuple *temp = _topLevelDeclarationTuples[viewIndex];
        _topLevelDeclarationTuples[viewIndex] = _topLevelDeclarationTuples[viewIndex + 1];
        _topLevelDeclarationTuples[viewIndex + 1] = temp;

    }

    [self updateConstraints];

    [UIView animateWithDuration:0.3 animations:^{
        [self layoutIfNeeded];
    }];
}

- (NSUInteger) indexForTopLevelDeclarationView: (UIView*) view {

    NSUInteger counter = 0;
    for (RACTuple *tuple in self.topLevelDeclarationTuples)
    {
        if(tuple.second == view)
            return counter;
        counter ++;
    }

    return NSIntegerMax;
}

#pragma mark - UIToolbarDelegate

- (UIBarPosition)positionForBar:(id <UIBarPositioning>)bar {
    return UIBarPositionTopAttached;
}


#pragma mark - Accessors
- (NSMutableArray *)topConstraints {
    if(!_topConstraints)
    {
        _topConstraints = [@[] mutableCopy];
    }

    return _topConstraints;
}

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