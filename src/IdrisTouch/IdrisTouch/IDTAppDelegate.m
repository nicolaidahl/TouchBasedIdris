//
//  IDTAppDelegate.m
//  IdrisTouch
//
//  Created by Nicolai Dahl on 05/03/14.
//  Copyright (c) 2014 Nicolai Dahl. All rights reserved.
//

#import "IDTAppDelegate.h"
#import "CASStyler.h"
#import "IDTMainViewController.h"
#import "DCIntrospect.h"
#import "CASUtilities.h"
#import "IDTAPIClient.h"
#import "IDTProgram.h"
#import "IDTTopLevelDec.h"
#import "IDTTopLevelDataDec.h"
#import "IDTConstructor.h"
#import "IDTTopLevelFuncDec.h"
#import "IDTClause.h"
#import "IDTConstantExpression.h"
#import "IDTConstantTypeType.h"
#import "IDTReference.h"

@implementation IDTAppDelegate

@synthesize managedObjectContext = _managedObjectContext, managedObjectModel = _managedObjectModel,
persistentStoreCoordinator = _persistentStoreCoordinator;

static NSString *const kNameOfStylesheetFile = @"Stylesheets/stylesheet.cas";

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions
{
    self.window = [[UIWindow alloc] initWithFrame:[[UIScreen mainScreen] bounds]];

    [self setupDCIntrospect];
    [self setupClassy];

    self.window.rootViewController = [IDTMainViewController new];

    self.window.backgroundColor = [UIColor whiteColor];

    //[self foolAroundWithAPI];

    [self.window makeKeyAndVisible];
    return YES;
}

- (void)foolAroundWithAPI {
    IDTAPIClient *client = [IDTAPIClient client];

    IDTProgram *program = [[IDTProgram alloc] initWithName:@"test"];

    IDTTopLevelDataDec *topLevelDataDec = [[IDTTopLevelDataDec alloc] init];
    topLevelDataDec.ident = @"Nat";
    topLevelDataDec.titype = [[IDTConstantExpression alloc] initWithConstant:[[IDTConstantTypeType alloc] init]];

    IDTConstructor *constructor = [[IDTConstructor alloc] init];
    constructor.constructor = @"Z";
    constructor.constructorType = [[IDTReference alloc] initWithVarName:@"Nat"];

    topLevelDataDec.constructors = [@[constructor] mutableCopy];

    IDTTopLevelFuncDec *funcDec = [[IDTTopLevelFuncDec alloc] init];
    funcDec.ident = @"zip";
    funcDec.titype = nil;
    
    IDTClause *clause = [[IDTClause alloc] init];

    funcDec.clauses = [@[clause] mutableCopy];

    program.topLevelDec = [@[topLevelDataDec] mutableCopy];

    RACSignal *signal = [client getEvaluationOfObjectHierarchy:program];

    [signal subscribeNext:^(NSDictionary *dictionary) {
        //IDTJSONSerializer *serializer = [IDTJSONSerializer serializer];
        //IDTProgram *program1 = [serializer deserializeToProgram:dictionary];
    } error:^(NSError *error) {

    } completed:^{

    }];
}

- (void)applicationWillResignActive:(UIApplication *)application
{
    // Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
    // Use this method to pause ongoing tasks, disable timers, and throttle down OpenGL ES frame rates. Games should use this method to pause the game.
}

- (void)applicationDidEnterBackground:(UIApplication *)application
{
    // Use this method to release shared resources, save user data, invalidate timers, and store enough application state information to restore your application to its current state in case it is terminated later. 
    // If your application supports background execution, this method is called instead of applicationWillTerminate: when the user quits.
}

- (void)applicationWillEnterForeground:(UIApplication *)application
{
    // Called as part of the transition from the background to the inactive state; here you can undo many of the changes made on entering the background.
}

- (void)applicationDidBecomeActive:(UIApplication *)application
{
    // Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
}

- (void)applicationWillTerminate:(UIApplication *)application
{
    // Saves changes in the application's managed object context before the application terminates.
    [self saveContext];
}

- (void)saveContext
{
    NSError *error = nil;
    NSManagedObjectContext *managedObjectContext = self.managedObjectContext;
    if (managedObjectContext != nil) {
        if ([managedObjectContext hasChanges] && ![managedObjectContext save:&error]) {
             // Replace this implementation with code to handle the error appropriately.
             // abort() causes the application to generate a crash log and terminate. You should not use this function in a shipping application, although it may be useful during development. 
            NSLog(@"Unresolved error %@, %@", error, [error userInfo]);
            abort();
        } 
    }
}

#pragma mark - Pods setup

- (void)setupDCIntrospect {
#if TARGET_IPHONE_SIMULATOR
    [[DCIntrospect sharedIntrospector] start];
#endif
}

- (void)setupClassy {
    NSError *error = nil;

    NSString *filePath = [[NSBundle mainBundle] pathForResource:[kNameOfStylesheetFile lastPathComponent] ofType:nil];
    [[CASStyler defaultStyler] setFilePath:filePath error:&error];
    if (error) {
        NSLog(@"Classy error : %@ -- file %@", [error localizedDescription],filePath);
    }
#if TARGET_IPHONE_SIMULATOR
    NSString *absoluteFilePath = CASAbsoluteFilePath(kNameOfStylesheetFile);
    [CASStyler defaultStyler].watchFilePath = absoluteFilePath;
#endif
}

#pragma mark - Core Data stack

// Returns the managed object context for the application.
// If the context doesn't already exist, it is created and bound to the persistent store coordinator for the application.
- (NSManagedObjectContext *)managedObjectContext
{
    if (_managedObjectContext != nil) {
        return _managedObjectContext;
    }
    
    NSPersistentStoreCoordinator *coordinator = [self persistentStoreCoordinator];
    if (coordinator != nil) {
        _managedObjectContext = [[NSManagedObjectContext alloc] init];
        [_managedObjectContext setPersistentStoreCoordinator:coordinator];
    }
    return _managedObjectContext;
}

// Returns the managed object model for the application.
// If the model doesn't already exist, it is created from the application's model.
- (NSManagedObjectModel *)managedObjectModel
{
    if (_managedObjectModel != nil) {
        return _managedObjectModel;
    }
    NSURL *modelURL = [[NSBundle mainBundle] URLForResource:@"IdrisTouch" withExtension:@"momd"];
    _managedObjectModel = [[NSManagedObjectModel alloc] initWithContentsOfURL:modelURL];
    return _managedObjectModel;
}

// Returns the persistent store coordinator for the application.
// If the coordinator doesn't already exist, it is created and the application's store added to it.
- (NSPersistentStoreCoordinator *)persistentStoreCoordinator
{
    if (_persistentStoreCoordinator != nil) {
        return _persistentStoreCoordinator;
    }
    
    NSURL *storeURL = [[self applicationDocumentsDirectory] URLByAppendingPathComponent:@"IdrisTouch.sqlite"];
    
    NSError *error = nil;
    _persistentStoreCoordinator = [[NSPersistentStoreCoordinator alloc] initWithManagedObjectModel:[self managedObjectModel]];
    if (![_persistentStoreCoordinator addPersistentStoreWithType:NSSQLiteStoreType configuration:nil URL:storeURL options:nil error:&error]) {
        /*
         Replace this implementation with code to handle the error appropriately.
         
         abort() causes the application to generate a crash log and terminate. You should not use this function in a shipping application, although it may be useful during development. 
         
         Typical reasons for an error here include:
         * The persistent store is not accessible;
         * The schema for the persistent store is incompatible with current managed object model.
         Check the error message to determine what the actual problem was.
         
         
         If the persistent store is not accessible, there is typically something wrong with the file path. Often, a file URL is pointing into the application's resources directory instead of a writeable directory.
         
         If you encounter schema incompatibility errors during development, you can reduce their frequency by:
         * Simply deleting the existing store:
         [[NSFileManager defaultManager] removeItemAtURL:storeURL error:nil]
         
         * Performing automatic lightweight migration by passing the following dictionary as the options parameter:
         @{NSMigratePersistentStoresAutomaticallyOption:@YES, NSInferMappingModelAutomaticallyOption:@YES}
         
         Lightweight migration will only work for a limited set of schema changes; consult "Core Data Model Versioning and Data Migration Programming Guide" for details.
         
         */
        NSLog(@"Unresolved error %@, %@", error, [error userInfo]);
        abort();
    }    
    
    return _persistentStoreCoordinator;
}

#pragma mark - Application's Documents directory

// Returns the URL to the application's Documents directory.
- (NSURL *)applicationDocumentsDirectory
{
    return [[[NSFileManager defaultManager] URLsForDirectory:NSDocumentDirectory inDomains:NSUserDomainMask] lastObject];
}

@end
