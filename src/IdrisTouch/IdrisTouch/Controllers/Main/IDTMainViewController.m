//
// Created by Nicolai Dahl on 05/03/14.
// Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTMainViewController.h"
#import "IDTAbstractViewModel.h"
#import "IDTMainViewModel.h"
#import "IDTMainView.h"
#import "IDTContextViewController.h"
#import "IDTProgram.h"
#import "IDTTopLevelDataDec.h"
#import "IDTDataDeclarationView.h"
#import "IDTTopLevelFuncDec.h"
#import "IDTFunctionDeclarationView.h"
#import "IDTConstructor.h"
#import "IDTClause.h"
#import "IDTConstantExpression.h"
#import "IDTConstantString.h"
#import "IDTInferenceRuleView.h"
#import "IDTNameTypeGroupInputView.h"
#import "IDTConstantTypeType.h"
#import "IDTPremissGroupInputView.h"
#import "IDTPi.h"
#import "IDTReference.h"
#import "IDTInputView.h"
#import "IDTDashedTextField.h"


@interface IDTMainViewController ()

@property (nonatomic, readonly) IDTMainView *mainView;
@property (nonatomic, readonly) IDTMainViewModel *viewModel;

@end

@implementation IDTMainViewController {
    IDTMainViewModel *_viewModel;
    IDTMainView *_mainView;
    UIPopoverController *_contextPopoverController;
}

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil {
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        _viewModel = [[IDTMainViewModel alloc] init];

        self.mainView.addTopLevelDecButton.rac_command = _viewModel.addTopLevelDecCommand;
        [[_viewModel.addTopLevelDecCommand.executionSignals flatten] subscribeNext:^(id x) {
            [self showContextPickerFromView: _mainView.addTopLevelDecButton];
        }];
    }

    return self;
}

- (void)showContextPickerFromView:(UIView *)view {

    IDTContextViewController *cvc = [[IDTContextViewController alloc] init];
    [[cvc.selectionCommand.executionSignals flatten] subscribeNext:^(NSNumber *index) {
        
        //Add data declaration
        if([index isEqualToNumber:@0]) 
        {
            IDTDataDeclarationView *dataView = [_mainView
                            addDataDeclaration].second;

            IDTTopLevelDataDec *dataDec = [[IDTTopLevelDataDec alloc] init];
            [self.viewModel.program.topLevelDec addObject:dataDec];

            [self bindDataDec:dataDec toDataDecView:dataView];

        }
        //Add function declaration
        else if([index isEqualToNumber:@1])
        {
            IDTFunctionDeclarationView *funcView = [_mainView
                            addFunctionDeclaration].second;

            IDTTopLevelFuncDec *funcDec = [[IDTTopLevelFuncDec alloc] init];
            funcDec.ident = @"zip";
            [self.viewModel.program.topLevelDec addObject:funcDec];

            [self bindFuncDec:funcDec toFuncDecView:funcView];

        }    

        [_contextPopoverController dismissPopoverAnimated:YES];
        _contextPopoverController = nil;

        [_mainView updateConstraints];
    }];

    _contextPopoverController = [[UIPopoverController alloc] initWithContentViewController:cvc];

    CGRect convertedRect = [view convertRect:view.bounds toView:self.mainView];
    [_contextPopoverController presentPopoverFromRect:convertedRect inView:self.mainView
                             permittedArrowDirections:UIPopoverArrowDirectionAny animated:YES];


}

- (void) bindDataDec: (IDTTopLevelDataDec *) dataDec toDataDecView: (IDTDataDeclarationView *) dataDecView
{
    RAC(dataDec, ident) = dataDecView.typeDeclaration.conclusionInputView.nameTextSignal;

    //The titype is bound to the premise -> conclusion
    RAC(dataDec, titype) = [dataDecView.typeDeclaration.premisesInputGroup.premissChangedSignal map:^id(NSString
    *text) {

        NSArray *textValues = [[[dataDecView.typeDeclaration.premisesInputGroup.inputViews.rac_sequence map:^id(IDTInputView *
        inputView) {
            return inputView.textField.text;
        }] filter:^BOOL(NSString *text) {
            return ![text isEqualToString:@""];
        }] array];

        IDTExpression *expression =
                [textValues.rac_sequence foldLeftWithStart:[[IDTConstantExpression alloc]
                initWithConstant:[IDTConstantTypeType new]]
                                                    reduce:^id(IDTPi *accumulator, NSString *rawString) {

                                                        IDTReference *refExpr = [[IDTReference alloc] initWithVarName:rawString];
                                                        return [[IDTPi alloc] initWithExpr1:refExpr andExpr2:accumulator];
                                                    }];

        return [RACSignal return:expression];
    }];

    //We react to new constructors by binding the inputview to a constructor type
    [[[dataDecView.addedNewConstructorCommand executionSignals] flatten] subscribeNext:^(IDTInferenceRuleView
    *constructorView) {
        IDTConstructor *constructor = [[IDTConstructor alloc] init];

        [dataDec.constructors addObject:constructor];

        [self bindConstructor:constructor toInferenceRuleView:constructorView];
    }];
}

- (void)bindFuncDec:(IDTTopLevelFuncDec *) funcDec toFuncDecView: (IDTFunctionDeclarationView *) funcDecView {
    [[funcDecView.addLineButton rac_signalForControlEvents:UIControlEventTouchUpInside] subscribeNext:^(id x) {
        IDTClause *clause = [[IDTClause alloc] init];
        clause.lhs = [@[] mutableCopy]; //TODO
        clause.rhs = [[IDTConstantExpression alloc] initWithConstant:[[IDTConstantString alloc] initWithString:@""]];
        //TODO

        [funcDec.clauses addObject:clause];
    }];
}

- (void) bindConstructor: (IDTConstructor *) constructor toInferenceRuleView: (IDTInferenceRuleView *) inferenceRuleView
{
    RAC(constructor, constructor) = inferenceRuleView.conclusionInputView.nameTextSignal;
    //The titype is bound to the premise -> conclusion
    RAC(constructor, constructorType) = [inferenceRuleView.premisesInputGroup.premissChangedSignal map:^id (NSString
    *text) {

        NSArray *textValues = [[[inferenceRuleView.premisesInputGroup.inputViews.rac_sequence map:^id(IDTInputView *
        inputView) {
            return inputView.textField.text;
        }] filter:^BOOL(NSString *text) {
            return ![text isEqualToString:@""];
        }] array];

        IDTExpression *expression =
                [textValues.rac_sequence foldLeftWithStart:[[IDTReference alloc] initWithVarName:inferenceRuleView.conclusionInputView
                                .typeInputView.textField.text]
                                                    reduce:^id(IDTPi *accumulator, NSString *rawString) {

                                                        IDTReference *refExpr = [[IDTReference alloc] initWithVarName:rawString];
                                                        return [[IDTPi alloc] initWithExpr1:refExpr andExpr2:accumulator];
                                                    }];

        return [RACSignal return:expression];
    }];


    //inferenceRuleView.conclusionInputView.typeTextSignal; //TODO use this for the conclusion
}

- (void)viewDidLoad {



    [super viewDidLoad];


}







#pragma mark - Accessors

- (IDTMainView *)mainView {
    if(!_mainView)
    {
        _mainView = [[IDTMainView alloc] initAndLayout];
    }
    
    return _mainView;
}

- (IDTMainViewModel *)viewModel {
    if(!_viewModel) 
    {
        _viewModel = [IDTMainViewModel new];
    }
    
    return _viewModel;
}


@end