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
#import "IDTNameTypeGroupInputView.h"
#import "IDTConstantTypeType.h"

#import "IDTPi.h"
#import "IDTReference.h"
#import "IDTClauseGroupInputView.h"
#import "IDTMetaVariable.h"
#import "IDTVariable.h"


@interface IDTMainViewController () <UIPopoverControllerDelegate>

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

            IDTContextViewController *cvc = [self configureContextPickerForTopDecs];
            [self showContextPopoverFromViewController:cvc fromView: _mainView.addTopLevelDecButton];
        }];
    }

    return self;
}

- (void)showContextPopoverFromViewController: (IDTContextViewController*) cvc fromView:(UIView *)view {

    [_contextPopoverController dismissPopoverAnimated:YES];
    _contextPopoverController = nil;

    _contextPopoverController = [[UIPopoverController alloc] initWithContentViewController:cvc];

    CGRect convertedRect = [view convertRect:view.bounds toView:self.mainView];
    [_contextPopoverController presentPopoverFromRect:convertedRect inView:self.mainView
                             permittedArrowDirections:UIPopoverArrowDirectionAny animated:YES];
    _contextPopoverController.delegate = self;

}

- (void)popoverControllerDidDismissPopover:(UIPopoverController *)popoverController {
    _contextPopoverController = nil;
}


- (IDTContextViewController *) configureContextPickerForTopDecs
{
    IDTContextViewController *cvc = [[IDTContextViewController alloc] initWithOptions:@[@"data", @"function"]];
    [[cvc.selectionCommand.executionSignals flatten] subscribeNext:^(NSNumber *index) {

        //Add data declaration
        if([index isEqualToNumber:@0])
        {
            IDTDataDeclarationView *dataView = [_mainView
                    addDataDeclaration].second;

            [self configureContextPickerForDataView:dataView];

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

        [self.mainView updateConstraints];

        [UIView animateWithDuration:0.2 animations:^{
            [self.mainView layoutIfNeeded];
        }];

    }];

    return cvc;
}

- (IDTContextViewController *) configureContextPickerForDataView: (IDTDataDeclarationView *) dataView
{
    NSArray *options = @[@"Nat"];
    IDTContextViewController *cvc = [[IDTContextViewController alloc] initWithOptions: options];

    [[dataView.typeDeclaration.premisesInputGroup.textChangedSignal flatten] subscribeNext:^(IDTTextFieldInputView
    *inputView) {

        [[cvc.selectionCommand.executionSignals flatten] subscribeNext:^(NSNumber *selection) {
            inputView.textField.text = options[(NSUInteger) [selection integerValue]];

            [_contextPopoverController dismissPopoverAnimated:YES];
            _contextPopoverController = nil;

        }];

        if(!_contextPopoverController && inputView)
            [self showContextPopoverFromViewController:cvc fromView:inputView];


    }];


    return cvc;
}

#pragma mark - Bind views to AST

- (void) bindDataDec: (IDTTopLevelDataDec *) dataDec toDataDecView: (IDTDataDeclarationView *) dataDecView
{
    RAC(dataDec, ident) = dataDecView.typeDeclaration.conclusionInputView.nameTextSignal;

    //The titype is bound to the premise -> conclusion
    IDTExpression *baseExpression = [[IDTConstantExpression alloc] initWithConstant:[IDTConstantTypeType new]];
    RAC(dataDec, titype) = [self foldContentOfTextFieldGroupInputView:dataDecView.typeDeclaration.premisesInputGroup
                                                       withBaseObject:baseExpression
                                                    andReductionBlock:[self piExpressionReductionBlock]
                                                  fromSignalOfSignals:dataDecView.typeDeclaration
                                                          .premisesInputGroup.textChangedSignal];

    //We react to new constructors by binding the inputview to a constructor type
    [[[dataDecView.addedNewConstructorCommand executionSignals] flatten] subscribeNext:^(IDTInferenceRuleView
    *constructorView) {
        IDTConstructor *constructor = [[IDTConstructor alloc] init];

        [dataDec.constructors addObject:constructor];

        [self bindConstructor:constructor toInferenceRuleView:constructorView];
    }];
}

- (void) bindConstructor: (IDTConstructor *) constructor toInferenceRuleView: (IDTInferenceRuleView *) inferenceRuleView
{
    RAC(constructor, constructor) = inferenceRuleView.conclusionInputView.nameTextSignal;
    //The titype is bound to the premise -> conclusion
    IDTExpression *baseExpression = [[IDTReference alloc] initWithVarName:inferenceRuleView.conclusionInputView
            .typeInputView.textField.text];
    RAC(constructor, constructorType) = [self foldContentOfTextFieldGroupInputView:inferenceRuleView.premisesInputGroup
                                                                    withBaseObject:baseExpression
                                                                 andReductionBlock:[self piExpressionReductionBlock]
                                                               fromSignalOfSignals:inferenceRuleView
                                                                       .premisesInputGroup
                                                                       .textChangedSignal];

    //inferenceRuleView.conclusionInputView.typeTextSignal; //TODO use this for the conclusion
}

- (void)bindFuncDec:(IDTTopLevelFuncDec *) funcDec toFuncDecView: (IDTFunctionDeclarationView *) funcDecView {
    funcDec.ident = @"zip";
    RAC(funcDec, titype) = [self foldContentOfTextFieldGroupInputView:funcDecView.typeDeclaration
                                                       withBaseObject:nil
                                                    andReductionBlock:[self piExpressionReductionBlock]
                                                  fromSignalOfSignals:funcDecView.typeDeclaration.textChangedSignal];

    [[[funcDecView.addedNewClauseCommand executionSignals] flatten] subscribeNext:^(IDTClauseGroupInputView *clauseGroupInputView) {
        IDTClause *clause = [[IDTClause alloc] init];

        [self bindClause: clause toClauseGroupInputView:clauseGroupInputView];

        [funcDec.clauses addObject:clause];
    }];
}




- (void)bindClause:(IDTClause *)clause toClauseGroupInputView:(IDTClauseGroupInputView *)view {

    id (^reductionBlock)(id, id) = ^id(NSMutableArray *accumulator, NSString *rawString) {
        [accumulator addObject:[[IDTVariable alloc] initWithName:rawString]];
        return accumulator;
    };

    RAC(clause,lhs) = [self foldContentOfTextFieldGroupInputView:view.lhs
                                                  withBaseObject:[@[] mutableCopy]
                                               andReductionBlock:reductionBlock fromSignalOfSignals:view.lhs
                    .textChangedSignal];
    RAC(clause, rhs) = [RACSignal return:[IDTMetaVariable new]];

}


#pragma mark - Helper methods

- (id (^)(id, id)) piExpressionReductionBlock
{
    return ^id(IDTPi *accumulator, NSString *rawString) {

        IDTReference *refExpr = [[IDTReference
                alloc] initWithVarName:rawString];
        return [[IDTPi alloc] initWithExpr1:refExpr
                                   andExpr2:accumulator];
    };
}

- (RACSignal *)foldContentOfTextFieldGroupInputView:(IDTTextFieldGroupInputView *)textGroupInputView
                                     withBaseObject:(id)baseObject
                                  andReductionBlock:(id (^)(id, id))reductionBlock
                                fromSignalOfSignals: (RACSignal*) signalOfSignals
{


    return [[[signalOfSignals flatten] ignore:nil]
            map:^id(NSString *text) {

                IDTExpression *be = baseObject;

                NSMutableArray *textValues = [[[[textGroupInputView.inputViews.rac_sequence map:^id(IDTTextFieldInputView *
                inputView) {
                    return inputView.textField.text;
                }] filter:^BOOL(NSString *text) {
                    return ![text isEqualToString:@""];
                }] array] mutableCopy];

                if(textValues.count > 0)
                {
                    if(!baseObject)
                    {
                        be = [[IDTReference alloc] initWithVarName: textValues.lastObject];
                        [textValues removeLastObject];
                    }

                    IDTExpression *expression = [textValues.rac_sequence foldLeftWithStart: be
                                                                          reduce:reductionBlock];
                    return [RACSignal return:expression];
                }

                return [RACSignal return:nil];
            }];
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