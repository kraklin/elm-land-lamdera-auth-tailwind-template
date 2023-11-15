# elm-land/lamdera/auth/tailwind
> Template for starting your next awesome project with [Lamdera](https://lamdera.com/), [Elm Land](https://elm.land) together with [Auth](https://github.com/lamdera/auth) and [Tailwind](https://tailwindcss.com/)

## Local development

### Setting up the environment
- create a project based on this template
- add the `googleAppClientId` and `googleAppClientSecret` that you get from the Google Console into `Env.elm`
- in the Google Console, add `http://localhost:8000/login/OAuthGoogle/callback` as the redirect URL
- `npm install`

### Starting the local dev servers

We need to start both `lamdera live` and `elm-land server` together with the watcher for `tailwind`. This is all done in one step with the awesome [run-pty](https://github.com/lydell/run-pty/) so all you need to do is just run

```sh
npm start
```

And open up the running project at `http://0.0.0.0:8000`
