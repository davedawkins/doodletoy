// ts2fable 0.7.1
module rec AppwriteSdk

open System
open Fable.Core
open Fable.Core.JS
open Fable.Core.JsInterop

// ++ Additions to ts2fable output ------------------------
//
type [<AllowNullLiteral>] Error = interface end
type URL = interface end
type File = interface end

let exports : IExports = importAll("appwrite")

let Create() = exports.Appwrite.Create()

type Preferences = obj

type HasId = interface end

type Session =
    abstract userId: string
    abstract provider: string
    abstract providerToken: string
    abstract providerUid: string
    inherit HasId

type User =
    abstract name: string
    abstract email: string
    abstract status: int
    abstract emailVerification: bool
    abstract registration: int
    abstract passwordUpdate: int
    abstract prefs: Preferences
    inherit HasId

[<AutoOpen>]
module Ext =
    type HasId with
        [<Emit("$0['$id']")>]
        member this._id : string = jsNative

type ListDocumentsResult<'T> =
    abstract sum : int
    abstract documents : 'T[]

// Other changes:
// - Change returned type to User and Session for appropriate calls
// - Removed optional arguments from createDocument
// - Convert "ResizeArray<string>" to "string array"
// - Change definition of Error from System.Exception to empty interface
// - createOauth2Session returns unit
// --------------------------------------------------------------------------


type [<AllowNullLiteral>] IExports =
    abstract AppwriteException: AppwriteExceptionStatic
    abstract Appwrite: AppwriteStatic

type [<AllowNullLiteral>] Headers =
    [<EmitIndexer>] abstract Item: key: string -> string with get, set

type [<AllowNullLiteral>] RealtimeResponseEvent<'T> =
    abstract ``event``: string with get, set
    abstract channels: string[] with get, set
    abstract timestamp: float with get, set
    abstract payload: 'T with get, set

type [<AllowNullLiteral>] AppwriteException =
    inherit Error
    abstract code: float with get, set
    abstract response: string with get, set

type [<AllowNullLiteral>] AppwriteExceptionStatic =
    [<EmitConstructor>] abstract Create: message: string * ?code: float * ?response: string -> AppwriteException

type [<AllowNullLiteral>] Appwrite =
    abstract config: AppwriteConfig with get, set
    abstract headers: Headers with get, set
    /// <summary>
    /// Set Endpoint
    ///
    /// Your project endpoint
    /// </summary>
    /// <param name="endpoint" />
    /// <returns />
    abstract setEndpoint: endpoint: string -> Appwrite
    /// <summary>Set Realtime Endpoint</summary>
    /// <param name="endpointRealtime" />
    /// <returns />
    abstract setEndpointRealtime: endpointRealtime: string -> Appwrite
    /// <summary>
    /// Set Project
    ///
    /// Your project ID
    /// </summary>
    /// <param name="value">string</param>
    /// <returns />
    abstract setProject: value: string -> Appwrite
    /// <summary>
    /// Set JWT
    ///
    /// Your secret JSON Web Token
    /// </summary>
    /// <param name="value">string</param>
    /// <returns />
    abstract setJWT: value: string -> Appwrite
    /// <summary>Set Locale</summary>
    /// <param name="value">string</param>
    /// <returns />
    abstract setLocale: value: string -> Appwrite
    /// <summary>Subscribes to Appwrite events and passes you the payload in realtime.</summary>
    /// <param name="channels">
    /// Channel to subscribe - pass a single channel as a string or multiple with an array of strings.
    ///
    /// Possible channels are:
    /// - account
    /// - collections
    /// - collections.[ID]
    /// - collections.[ID].documents
    /// - documents
    /// - documents.[ID]
    /// - files
    /// - files.[ID]
    /// - executions
    /// - executions.[ID]
    /// - functions.[ID]
    /// - teams
    /// - teams.[ID]
    /// - memberships
    /// - memberships.[ID]
    /// </param>
    /// <param name="callback">Is called on every realtime update.</param>
    /// <returns>Unsubscribes from events.</returns>
    abstract subscribe: channels: U2<string, string[]> * callback: (RealtimeResponseEvent<'T> -> unit) -> (unit -> unit)
    abstract account: AppwriteAccount<'T, 'T_1, 'T_2, 'T_3, 'T_4, 'T_5, 'T_6, 'T_7, 'T_8, 'T_9, 'T_10, 'T_11, 'T_12, 'T_13, 'T_14, 'T_15, 'T_16, 'T_17, 'T_18, 'T_19, 'T_20, 'T_21> with get, set
    abstract avatars: AppwriteAvatars with get, set
    abstract database: AppwriteDatabase<'T> with get, set
    abstract functions: AppwriteFunctions<'T, 'T_1, 'T_2> with get, set
    abstract locale: AppwriteLocale<'T, 'T_1, 'T_2, 'T_3, 'T_4, 'T_5, 'T_6> with get, set
    abstract storage: AppwriteStorage<'T, 'T_1, 'T_2, 'T_3, 'T_4> with get, set
    abstract teams: AppwriteTeams<'T, 'T_1, 'T_2, 'T_3, 'T_4, 'T_5, 'T_6, 'T_7, 'T_8, 'T_9> with get, set

type [<AllowNullLiteral>] AppwriteStatic =
    [<EmitConstructor>] abstract Create: unit -> Appwrite

type [<AllowNullLiteral>] AppwriteConfig =
    abstract endpoint: string with get, set
    abstract endpointRealtime: string with get, set
    abstract project: string with get, set
    abstract jwt: string with get, set
    abstract locale: string with get, set

type [<AllowNullLiteral>] AppwriteAccount<'T, 'T_1, 'T_2, 'T_3, 'T_4, 'T_5, 'T_6, 'T_7, 'T_8, 'T_9, 'T_10, 'T_11, 'T_12, 'T_13, 'T_14, 'T_15, 'T_16, 'T_17, 'T_18, 'T_19, 'T_20, 'T_21> =
    /// <summary>
    /// Get Account
    ///
    /// Get currently logged in user data as JSON object.
    /// </summary>
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract get: unit -> Promise<User>
    /// <summary>
    /// Create Account
    ///
    /// Use this endpoint to allow a new user to register a new account in your
    /// project. After the user registration completes successfully, you can use
    /// the <see cref="/docs/client/account.accountCreateVerification">/account/verfication</see>
    /// route to start verifying the user email address. To allow the new user to
    /// login to their new account, you need to create a new [account
    /// session](/docs/client/account#accountCreateSession).
    /// </summary>
    /// <param name="email" />
    /// <param name="password" />
    /// <param name="name" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract create: email: string * password: string * ?name: string -> Promise<'T_1>
    /// <summary>
    /// Delete Account
    ///
    /// Delete a currently logged in user account. Behind the scene, the user
    /// record is not deleted but permanently blocked from any access. This is done
    /// to avoid deleted accounts being overtaken by new users with the same email
    /// address. Any user-related resources like documents or storage files should
    /// be deleted separately.
    /// </summary>
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract delete: unit -> Promise<'T_2>
    /// <summary>
    /// Update Account Email
    ///
    /// Update currently logged in user account email address. After changing user
    /// address, user confirmation status is being reset and a new confirmation
    /// mail is sent. For security measures, user password is required to complete
    /// this request.
    /// This endpoint can also be used to convert an anonymous account to a normal
    /// one, by passing an email address and a new password.
    /// </summary>
    /// <param name="email" />
    /// <param name="password" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract updateEmail: email: string * password: string -> Promise<'T_3>
    /// <summary>
    /// Create Account JWT
    ///
    /// Use this endpoint to create a JSON Web Token. You can use the resulting JWT
    /// to authenticate on behalf of the current user when working with the
    /// Appwrite server-side API and SDKs. The JWT secret is valid for 15 minutes
    /// from its creation and will be invalid if the user will logout in that time
    /// frame.
    /// </summary>
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract createJWT: unit -> Promise<'T_4>
    /// <summary>
    /// Get Account Logs
    ///
    /// Get currently logged in user list of latest security activity logs. Each
    /// log returns user IP address, location and date and time of log.
    /// </summary>
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract getLogs: unit -> Promise<'T_5>
    /// <summary>
    /// Update Account Name
    ///
    /// Update currently logged in user account name.
    /// </summary>
    /// <param name="name" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract updateName: name: string -> Promise<'T_6>
    /// <summary>
    /// Update Account Password
    ///
    /// Update currently logged in user password. For validation, user is required
    /// to pass in the new password, and the old password. For users created with
    /// OAuth and Team Invites, oldPassword is optional.
    /// </summary>
    /// <param name="password" />
    /// <param name="oldPassword" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract updatePassword: password: string * ?oldPassword: string -> Promise<'T_7>
    /// <summary>
    /// Get Account Preferences
    ///
    /// Get currently logged in user preferences as a key-value object.
    /// </summary>
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract getPrefs: unit -> Promise<'T_8>
    /// <summary>
    /// Update Account Preferences
    ///
    /// Update currently logged in user account preferences. You can pass only the
    /// specific settings you wish to update.
    /// </summary>
    /// <param name="prefs" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract updatePrefs: prefs: obj -> Promise<'T_9>
    /// <summary>
    /// Create Password Recovery
    ///
    /// Sends the user an email with a temporary secret key for password reset.
    /// When the user clicks the confirmation link he is redirected back to your
    /// app password reset URL with the secret key and email address values
    /// attached to the URL query string. Use the query string params to submit a
    /// request to the [PUT
    /// /account/recovery](/docs/client/account#accountUpdateRecovery) endpoint to
    /// complete the process. The verification link sent to the user's email
    /// address is valid for 1 hour.
    /// </summary>
    /// <param name="email" />
    /// <param name="url" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract createRecovery: email: string * url: string -> Promise<'T_10>
    /// <summary>
    /// Create Password Recovery (confirmation)
    ///
    /// Use this endpoint to complete the user account password reset. Both the
    /// **userId** and **secret** arguments will be passed as query parameters to
    /// the redirect URL you have provided when sending your request to the [POST
    /// /account/recovery](/docs/client/account#accountCreateRecovery) endpoint.
    ///
    /// Please note that in order to avoid a [Redirect
    /// Attack](<see href="https://github.com/OWASP/CheatSheetSeries/blob/master/cheatsheets/Unvalidated_Redirects_and_Forwards_Cheat_Sheet.md)" />
    /// the only valid redirect URLs are the ones from domains you have set when
    /// adding your platforms in the console interface.
    /// </summary>
    /// <param name="userId" />
    /// <param name="secret" />
    /// <param name="password" />
    /// <param name="passwordAgain" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract updateRecovery: userId: string * secret: string * password: string * passwordAgain: string -> Promise<'T_11>
    /// <summary>
    /// Get Account Sessions
    ///
    /// Get currently logged in user list of active sessions across different
    /// devices.
    /// </summary>
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract getSessions: unit -> Promise<'T_12>
    /// <summary>
    /// Create Account Session
    ///
    /// Allow the user to login into their account by providing a valid email and
    /// password combination. This route will create a new session for the user.
    /// </summary>
    /// <param name="email" />
    /// <param name="password" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract createSession: email: string * password: string -> Promise<Session>
    /// <summary>
    /// Delete All Account Sessions
    ///
    /// Delete all sessions from the user account and remove any sessions cookies
    /// from the end client.
    /// </summary>
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract deleteSessions: unit -> Promise<'T_14>
    /// <summary>
    /// Create Anonymous Session
    ///
    /// Use this endpoint to allow a new user to register an anonymous account in
    /// your project. This route will also create a new session for the user. To
    /// allow the new user to convert an anonymous account to a normal account, you
    /// need to update its [email and
    /// password](/docs/client/account#accountUpdateEmail) or create an [OAuth2
    /// session](/docs/client/account#accountCreateOAuth2Session).
    /// </summary>
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract createAnonymousSession: unit -> Promise<'T_15>
    /// <summary>
    /// Create Magic URL session
    ///
    /// Sends the user an email with a secret key for creating a session. When the
    /// user clicks the link in the email, the user is redirected back to the URL
    /// you provided with the secret key and userId values attached to the URL
    /// query string. Use the query string parameters to submit a request to the
    /// [PUT
    /// /account/sessions/magic-url](/docs/client/account#accountUpdateMagicURLSession)
    /// endpoint to complete the login process. The link sent to the user's email
    /// address is valid for 1 hour. If you are on a mobile device you can leave
    /// the URL parameter empty, so that the login completion will be handled by
    /// your Appwrite instance by default.
    /// </summary>
    /// <param name="email" />
    /// <param name="url" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract createMagicURLSession: email: string * ?url: string -> Promise<'T_16>
    /// <summary>
    /// Create Magic URL session (confirmation)
    ///
    /// Use this endpoint to complete creating the session with the Magic URL. Both
    /// the **userId** and **secret** arguments will be passed as query parameters
    /// to the redirect URL you have provided when sending your request to the
    /// [POST
    /// /account/sessions/magic-url](/docs/client/account#accountCreateMagicURLSession)
    /// endpoint.
    ///
    /// Please note that in order to avoid a [Redirect
    /// Attack](<see href="https://github.com/OWASP/CheatSheetSeries/blob/master/cheatsheets/Unvalidated_Redirects_and_Forwards_Cheat_Sheet.md)" />
    /// the only valid redirect URLs are the ones from domains you have set when
    /// adding your platforms in the console interface.
    /// </summary>
    /// <param name="userId" />
    /// <param name="secret" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract updateMagicURLSession: userId: string * secret: string -> Promise<'T_17>
    /// <summary>
    /// Create Account Session with OAuth2
    ///
    /// Allow the user to login to their account using the OAuth2 provider of their
    /// choice. Each OAuth2 provider should be enabled from the Appwrite console
    /// first. Use the success and failure arguments to provide a redirect URL's
    /// back to your app when login is completed.
    ///
    /// If there is already an active session, the new session will be attached to
    /// the logged-in account. If there are no active sessions, the server will
    /// attempt to look for a user with the same email address as the email
    /// received from the OAuth2 provider and attach the new session to the
    /// existing user. If no matching user is found - the server will create a new
    /// user..
    /// </summary>
    /// <param name="provider" />
    /// <param name="success" />
    /// <param name="failure" />
    /// <param name="scopes" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract createOAuth2Session: provider: string * ?success: string * ?failure: string * ?scopes: string[] -> U2<unit, URL>
    /// <summary>
    /// Get Session By ID
    ///
    /// Use this endpoint to get a logged in user's session using a Session ID.
    /// Inputting 'current' will return the current session being used.
    /// </summary>
    /// <param name="sessionId" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract getSession: sessionId: string -> Promise<'T_18>
    /// <summary>
    /// Delete Account Session
    ///
    /// Use this endpoint to log out the currently logged in user from all their
    /// account sessions across all of their different devices. When using the
    /// option id argument, only the session unique ID provider will be deleted.
    /// </summary>
    /// <param name="sessionId" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract deleteSession: sessionId: string -> Promise<'T_19>
    /// <summary>
    /// Create Email Verification
    ///
    /// Use this endpoint to send a verification message to your user email address
    /// to confirm they are the valid owners of that address. Both the **userId**
    /// and **secret** arguments will be passed as query parameters to the URL you
    /// have provided to be attached to the verification email. The provided URL
    /// should redirect the user back to your app and allow you to complete the
    /// verification process by verifying both the **userId** and **secret**
    /// parameters. Learn more about how to [complete the verification
    /// process](/docs/client/account#accountUpdateVerification). The verification
    /// link sent to the user's email address is valid for 7 days.
    ///
    /// Please note that in order to avoid a [Redirect
    /// Attack](<see href="https://github.com/OWASP/CheatSheetSeries/blob/master/cheatsheets/Unvalidated_Redirects_and_Forwards_Cheat_Sheet.md)," />
    /// the only valid redirect URLs are the ones from domains you have set when
    /// adding your platforms in the console interface.
    /// </summary>
    /// <param name="url" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract createVerification: url: string -> Promise<'T_20>
    /// <summary>
    /// Create Email Verification (confirmation)
    ///
    /// Use this endpoint to complete the user email verification process. Use both
    /// the **userId** and **secret** parameters that were attached to your app URL
    /// to verify the user email ownership. If confirmed this route will return a
    /// 200 status code.
    /// </summary>
    /// <param name="userId" />
    /// <param name="secret" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract updateVerification: userId: string * secret: string -> Promise<'T_21>

type [<AllowNullLiteral>] AppwriteAvatars =
    /// <summary>
    /// Get Browser Icon
    ///
    /// You can use this endpoint to show different browser icons to your users.
    /// The code argument receives the browser code as it appears in your user
    /// /account/sessions endpoint. Use width, height and quality arguments to
    /// change the output settings.
    /// </summary>
    /// <param name="code" />
    /// <param name="width" />
    /// <param name="height" />
    /// <param name="quality" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract getBrowser: code: string * ?width: float * ?height: float * ?quality: float -> URL
    /// <summary>
    /// Get Credit Card Icon
    ///
    /// The credit card endpoint will return you the icon of the credit card
    /// provider you need. Use width, height and quality arguments to change the
    /// output settings.
    /// </summary>
    /// <param name="code" />
    /// <param name="width" />
    /// <param name="height" />
    /// <param name="quality" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract getCreditCard: code: string * ?width: float * ?height: float * ?quality: float -> URL
    /// <summary>
    /// Get Favicon
    ///
    /// Use this endpoint to fetch the favorite icon (AKA favicon) of any remote
    /// website URL.
    /// </summary>
    /// <param name="url" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract getFavicon: url: string -> URL
    /// <summary>
    /// Get Country Flag
    ///
    /// You can use this endpoint to show different country flags icons to your
    /// users. The code argument receives the 2 letter country code. Use width,
    /// height and quality arguments to change the output settings.
    /// </summary>
    /// <param name="code" />
    /// <param name="width" />
    /// <param name="height" />
    /// <param name="quality" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract getFlag: code: string * ?width: float * ?height: float * ?quality: float -> URL
    /// <summary>
    /// Get Image from URL
    ///
    /// Use this endpoint to fetch a remote image URL and crop it to any image size
    /// you want. This endpoint is very useful if you need to crop and display
    /// remote images in your app or in case you want to make sure a 3rd party
    /// image is properly served using a TLS protocol.
    /// </summary>
    /// <param name="url" />
    /// <param name="width" />
    /// <param name="height" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract getImage: url: string * ?width: float * ?height: float -> URL
    /// <summary>
    /// Get User Initials
    ///
    /// Use this endpoint to show your user initials avatar icon on your website or
    /// app. By default, this route will try to print your logged-in user name or
    /// email initials. You can also overwrite the user name if you pass the 'name'
    /// parameter. If no name is given and no user is logged, an empty avatar will
    /// be returned.
    ///
    /// You can use the color and background params to change the avatar colors. By
    /// default, a random theme will be selected. The random theme will persist for
    /// the user's initials when reloading the same theme will always return for
    /// the same initials.
    /// </summary>
    /// <param name="name" />
    /// <param name="width" />
    /// <param name="height" />
    /// <param name="color" />
    /// <param name="background" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract getInitials: ?name: string * ?width: float * ?height: float * ?color: string * ?background: string -> URL
    /// <summary>
    /// Get QR Code
    ///
    /// Converts a given plain text to a QR code image. You can use the query
    /// parameters to change the size and style of the resulting image.
    /// </summary>
    /// <param name="text" />
    /// <param name="size" />
    /// <param name="margin" />
    /// <param name="download" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract getQR: text: string * ?size: float * ?margin: float * ?download: bool -> URL

type [<AllowNullLiteral>] AppwriteDatabase<'Doc> =
    /// <summary>
    /// List Documents
    ///
    /// Get a list of all the user documents. You can use the query params to
    /// filter your results. On admin mode, this endpoint will return a list of all
    /// of the project's documents. [Learn more about different API
    /// modes](/docs/admin).
    /// </summary>
    /// <param name="collectionId" />
    /// <param name="filters" />
    /// <param name="limit" />
    /// <param name="offset" />
    /// <param name="orderField" />
    /// <param name="orderType" />
    /// <param name="orderCast" />
    /// <param name="search" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract listDocuments: collectionId: string * ?filters: string[] * ?limit: float * ?offset: float * ?orderField: string * ?orderType: string * ?orderCast: string * ?search: string -> Promise<ListDocumentsResult<'Doc>>
    /// <summary>
    /// Create Document
    ///
    /// Create a new Document. Before using this route, you should create a new
    /// collection resource using either a [server
    /// integration](/docs/server/database#databaseCreateCollection) API or
    /// directly from your database console.
    /// </summary>
    /// <param name="collectionId" />
    /// <param name="data" />
    /// <param name="read" />
    /// <param name="write" />
    /// <param name="parentDocument" />
    /// <param name="parentProperty" />
    /// <param name="parentPropertyType" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract createDocument: collectionId: string * data: obj * ?read: string[] * ?write: string[] * ?parentDocument: string * ?parentProperty: string * ?parentPropertyType: string -> Promise<'Doc>
    /// <summary>
    /// Get Document
    ///
    /// Get a document by its unique ID. This endpoint response returns a JSON
    /// object with the document data.
    /// </summary>
    /// <param name="collectionId" />
    /// <param name="documentId" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract getDocument: collectionId: string * documentId: string -> Promise<'Doc>
    /// <summary>
    /// Update Document
    ///
    /// Update a document by its unique ID. Using the patch method you can pass
    /// only specific fields that will get updated.
    /// </summary>
    /// <param name="collectionId" />
    /// <param name="documentId" />
    /// <param name="data" />
    /// <param name="read" />
    /// <param name="write" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract updateDocument: collectionId: string * documentId: string * data: obj * ?read: string[] * ?write: string[] -> Promise<'Doc>
    /// <summary>
    /// Delete Document
    ///
    /// Delete a document by its unique ID. This endpoint deletes only the parent
    /// documents, its attributes and relations to other documents. Child documents
    /// **will not** be deleted.
    /// </summary>
    /// <param name="collectionId" />
    /// <param name="documentId" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract deleteDocument: collectionId: string * documentId: string -> Promise<unit>

type [<AllowNullLiteral>] AppwriteFunctions<'T, 'T_1, 'T_2> =
    /// <summary>
    /// List Executions
    ///
    /// Get a list of all the current user function execution logs. You can use the
    /// query params to filter your results. On admin mode, this endpoint will
    /// return a list of all of the project's executions. [Learn more about
    /// different API modes](/docs/admin).
    /// </summary>
    /// <param name="functionId" />
    /// <param name="search" />
    /// <param name="limit" />
    /// <param name="offset" />
    /// <param name="orderType" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract listExecutions: functionId: string * ?search: string * ?limit: float * ?offset: float * ?orderType: string -> Promise<'T>
    /// <summary>
    /// Create Execution
    ///
    /// Trigger a function execution. The returned object will return you the
    /// current execution status. You can ping the <c>Get Execution</c> endpoint to get
    /// updates on the current execution status. Once this endpoint is called, your
    /// function execution process will start asynchronously.
    /// </summary>
    /// <param name="functionId" />
    /// <param name="data" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract createExecution: functionId: string * ?data: string -> Promise<'T_1>
    /// <summary>
    /// Get Execution
    ///
    /// Get a function execution log by its unique ID.
    /// </summary>
    /// <param name="functionId" />
    /// <param name="executionId" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract getExecution: functionId: string * executionId: string -> Promise<'T_2>

type [<AllowNullLiteral>] AppwriteLocale<'T, 'T_1, 'T_2, 'T_3, 'T_4, 'T_5, 'T_6> =
    /// <summary>
    /// Get User Locale
    ///
    /// Get the current user location based on IP. Returns an object with user
    /// country code, country name, continent name, continent code, ip address and
    /// suggested currency. You can use the locale header to get the data in a
    /// supported language.
    ///
    /// (<see href="https://db-ip.com">IP Geolocation by DB-IP</see>)
    /// </summary>
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract get: unit -> Promise<'T>
    /// <summary>
    /// List Continents
    ///
    /// List of all continents. You can use the locale header to get the data in a
    /// supported language.
    /// </summary>
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract getContinents: unit -> Promise<'T_1>
    /// <summary>
    /// List Countries
    ///
    /// List of all countries. You can use the locale header to get the data in a
    /// supported language.
    /// </summary>
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract getCountries: unit -> Promise<'T_2>
    /// <summary>
    /// List EU Countries
    ///
    /// List of all countries that are currently members of the EU. You can use the
    /// locale header to get the data in a supported language.
    /// </summary>
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract getCountriesEU: unit -> Promise<'T_3>
    /// <summary>
    /// List Countries Phone Codes
    ///
    /// List of all countries phone codes. You can use the locale header to get the
    /// data in a supported language.
    /// </summary>
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract getCountriesPhones: unit -> Promise<'T_4>
    /// <summary>
    /// List Currencies
    ///
    /// List of all currencies, including currency symbol, name, plural, and
    /// decimal digits for all major and minor currencies. You can use the locale
    /// header to get the data in a supported language.
    /// </summary>
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract getCurrencies: unit -> Promise<'T_5>
    /// <summary>
    /// List Languages
    ///
    /// List of all languages classified by ISO 639-1 including 2-letter code, name
    /// in English, and name in the respective language.
    /// </summary>
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract getLanguages: unit -> Promise<'T_6>

type [<AllowNullLiteral>] AppwriteStorage<'T, 'T_1, 'T_2, 'T_3, 'T_4> =
    /// <summary>
    /// List Files
    ///
    /// Get a list of all the user files. You can use the query params to filter
    /// your results. On admin mode, this endpoint will return a list of all of the
    /// project's files. <see cref="/docs/admin">Learn more about different API modes</see>.
    /// </summary>
    /// <param name="search" />
    /// <param name="limit" />
    /// <param name="offset" />
    /// <param name="orderType" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract listFiles: ?search: string * ?limit: float * ?offset: float * ?orderType: string -> Promise<'T>
    /// <summary>
    /// Create File
    ///
    /// Create a new file. The user who creates the file will automatically be
    /// assigned to read and write access unless he has passed custom values for
    /// read and write arguments.
    /// </summary>
    /// <param name="file" />
    /// <param name="read" />
    /// <param name="write" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract createFile: file: File * ?read: string[] * ?write: string[] -> Promise<'T_1>
    /// <summary>
    /// Get File
    ///
    /// Get a file by its unique ID. This endpoint response returns a JSON object
    /// with the file metadata.
    /// </summary>
    /// <param name="fileId" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract getFile: fileId: string -> Promise<'T_2>
    /// <summary>
    /// Update File
    ///
    /// Update a file by its unique ID. Only users with write permissions have
    /// access to update this resource.
    /// </summary>
    /// <param name="fileId" />
    /// <param name="read" />
    /// <param name="write" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract updateFile: fileId: string * read: string[] * write: string[] -> Promise<'T_3>
    /// <summary>
    /// Delete File
    ///
    /// Delete a file by its unique ID. Only users with write permissions have
    /// access to delete this resource.
    /// </summary>
    /// <param name="fileId" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract deleteFile: fileId: string -> Promise<'T_4>
    /// <summary>
    /// Get File for Download
    ///
    /// Get a file content by its unique ID. The endpoint response return with a
    /// 'Content-Disposition: attachment' header that tells the browser to start
    /// downloading the file to user downloads directory.
    /// </summary>
    /// <param name="fileId" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract getFileDownload: fileId: string -> URL
    /// <summary>
    /// Get File Preview
    ///
    /// Get a file preview image. Currently, this method supports preview for image
    /// files (jpg, png, and gif), other supported formats, like pdf, docs, slides,
    /// and spreadsheets, will return the file icon image. You can also pass query
    /// string arguments for cutting and resizing your preview image.
    /// </summary>
    /// <param name="fileId" />
    /// <param name="width" />
    /// <param name="height" />
    /// <param name="gravity" />
    /// <param name="quality" />
    /// <param name="borderWidth" />
    /// <param name="borderColor" />
    /// <param name="borderRadius" />
    /// <param name="opacity" />
    /// <param name="rotation" />
    /// <param name="background" />
    /// <param name="output" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract getFilePreview: fileId: string * ?width: float * ?height: float * ?gravity: string * ?quality: float * ?borderWidth: float * ?borderColor: string * ?borderRadius: float * ?opacity: float * ?rotation: float * ?background: string * ?output: string -> URL
    /// <summary>
    /// Get File for View
    ///
    /// Get a file content by its unique ID. This endpoint is similar to the
    /// download method but returns with no  'Content-Disposition: attachment'
    /// header.
    /// </summary>
    /// <param name="fileId" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract getFileView: fileId: string -> URL

type [<AllowNullLiteral>] AppwriteTeams<'T, 'T_1, 'T_2, 'T_3, 'T_4, 'T_5, 'T_6, 'T_7, 'T_8, 'T_9> =
    /// <summary>
    /// List Teams
    ///
    /// Get a list of all the current user teams. You can use the query params to
    /// filter your results. On admin mode, this endpoint will return a list of all
    /// of the project's teams. [Learn more about different API
    /// modes](/docs/admin).
    /// </summary>
    /// <param name="search" />
    /// <param name="limit" />
    /// <param name="offset" />
    /// <param name="orderType" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract list: ?search: string * ?limit: float * ?offset: float * ?orderType: string -> Promise<'T>
    /// <summary>
    /// Create Team
    ///
    /// Create a new team. The user who creates the team will automatically be
    /// assigned as the owner of the team. The team owner can invite new members,
    /// who will be able add new owners and update or delete the team from your
    /// project.
    /// </summary>
    /// <param name="name" />
    /// <param name="roles" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract create: name: string * ?roles: string[] -> Promise<'T_1>
    /// <summary>
    /// Get Team
    ///
    /// Get a team by its unique ID. All team members have read access for this
    /// resource.
    /// </summary>
    /// <param name="teamId" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract get: teamId: string -> Promise<'T_2>
    /// <summary>
    /// Update Team
    ///
    /// Update a team by its unique ID. Only team owners have write access for this
    /// resource.
    /// </summary>
    /// <param name="teamId" />
    /// <param name="name" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract update: teamId: string * name: string -> Promise<'T_3>
    /// <summary>
    /// Delete Team
    ///
    /// Delete a team by its unique ID. Only team owners have write access for this
    /// resource.
    /// </summary>
    /// <param name="teamId" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract delete: teamId: string -> Promise<'T_4>
    /// <summary>
    /// Get Team Memberships
    ///
    /// Get a team members by the team unique ID. All team members have read access
    /// for this list of resources.
    /// </summary>
    /// <param name="teamId" />
    /// <param name="search" />
    /// <param name="limit" />
    /// <param name="offset" />
    /// <param name="orderType" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract getMemberships: teamId: string * ?search: string * ?limit: float * ?offset: float * ?orderType: string -> Promise<'T_5>
    /// <summary>
    /// Create Team Membership
    ///
    /// Use this endpoint to invite a new member to join your team. If initiated
    /// from Client SDK, an email with a link to join the team will be sent to the
    /// new member's email address if the member doesn't exist in the project it
    /// will be created automatically. If initiated from server side SDKs, new
    /// member will automatically be added to the team.
    ///
    /// Use the 'URL' parameter to redirect the user from the invitation email back
    /// to your app. When the user is redirected, use the [Update Team Membership
    /// Status](/docs/client/teams#teamsUpdateMembershipStatus) endpoint to allow
    /// the user to accept the invitation to the team.  While calling from side
    /// SDKs the redirect url can be empty string.
    ///
    /// Please note that in order to avoid a [Redirect
    /// Attacks](<see href="https://github.com/OWASP/CheatSheetSeries/blob/master/cheatsheets/Unvalidated_Redirects_and_Forwards_Cheat_Sheet.md)" />
    /// the only valid redirect URL's are the once from domains you have set when
    /// added your platforms in the console interface.
    /// </summary>
    /// <param name="teamId" />
    /// <param name="email" />
    /// <param name="roles" />
    /// <param name="url" />
    /// <param name="name" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract createMembership: teamId: string * email: string * roles: string[] * url: string * ?name: string -> Promise<'T_6>
    /// <summary>Update Membership Roles</summary>
    /// <param name="teamId" />
    /// <param name="membershipId" />
    /// <param name="roles" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract updateMembershipRoles: teamId: string * membershipId: string * roles: string[] -> Promise<'T_7>
    /// <summary>
    /// Delete Team Membership
    ///
    /// This endpoint allows a user to leave a team or for a team owner to delete
    /// the membership of any other team member. You can also use this endpoint to
    /// delete a user membership even if it is not accepted.
    /// </summary>
    /// <param name="teamId" />
    /// <param name="membershipId" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract deleteMembership: teamId: string * membershipId: string -> Promise<'T_8>
    /// <summary>
    /// Update Team Membership Status
    ///
    /// Use this endpoint to allow a user to accept an invitation to join a team
    /// after being redirected back to your app from the invitation email recieved
    /// by the user.
    /// </summary>
    /// <param name="teamId" />
    /// <param name="membershipId" />
    /// <param name="userId" />
    /// <param name="secret" />
    /// <exception cref="AppwriteException" />
    /// <returns />
    abstract updateMembershipStatus: teamId: string * membershipId: string * userId: string * secret: string -> Promise<'T_9>