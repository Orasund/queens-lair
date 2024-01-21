import { registerAllSounds, playSound } from "./sound.js"

var app = Elm.Main.init({
    node:
        document.querySelector('main')
})

registerAllSounds(["theme.mp3"])

app.ports.playTheme.subscribe(function
    (message) {
    playSound("theme.mp3", true, () => { })
});

if (typeof navigator.serviceWorker !== 'undefined') {
    navigator.serviceWorker
        .register('/queens-lair/service_worker.js',
            { scope: '/queens-lair/' })
        .then(function () { console.log("Service Worker Registered"); });
}
// you can use ports and stuff here