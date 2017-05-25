# #lofi in Elm

Parse [#lofi](https://lofi.design/) content. Also available in [JavaScript](https://github.com/RoyalIcing/lofi-js) and [Elixir](https://github.com/RoyalIcing/lofi-elixir)

## Basic usage

```elm
primaryButton = "Click me #button #primary"
primaryButtonLofi = Lofi.parseElement primaryButton
```

```elm
dangerButtonLofi = Lofi.parseElement "Click me #button #primary #alternative: danger"
```

```elm
buttonMentioningTitleLofi = Lofi.parseElement "@title #button #primary"
```

```elm
welcomeMessageLofi = Lofi.parseElement "Hello @user.firstName #primary"
```
