# Clientside Jitsi Hacks

Collection of client side jitsi customizations.

## Raised Hand

Paste this into JS console (developer tools, Ctrl+Shift+I in firefox or chromium) after creating / joining the conference, to collect "raised hand" events in a list.

Useful for classes or other presentations if you are the presenter and want a list of people who raised their hand to ask questions.

(Normally, the "raised hand" just results in a transient notification and a tiny icon, both of which are too easy to overlook).

To make the names disappear, click on them.

Yes I know the layout isn't very sophisticated but it works.

```var myDiv=document.createElement('div');document.getElementsByTagName('body')[0].appendChild(myDiv);myDiv.style.width="160px";myDiv.style.height="480px";myDiv.style.background="lightred";myDiv.style.backgroundColor="black";myDiv.style.color="white";myDiv.style.position="absolute";myDiv.style.border="6px solid #73AD21";myDiv.style.zIndex="4";myDiv.style.opacity="0.6";myDiv.innerHTML="<span>Raised Hands</span>";var raisedHandCtr=0;APP.conference._room.eventEmitter.addListener('conference.participant_property_changed',function(x,y){if(y=='raisedHand' && APP.conference._room.participants[x["_id"]]._properties.raisedHand=='true'){myDiv.innerHTML+="<span width='100%' display='flex' style='font-size:x-large;color:yellow;background:#606020' id='raisedHandNo"+raisedHandCtr+"' onmouseover=\"this.style.color='white';this.style.background='orange'\" onmouseout=\"this.style.color='yellow';this.style.background='#606020'\" onclick=\"document.querySelector('#raisedHandNo"+raisedHandCtr+"').remove()\"><br/>"+x["_displayName"]+"</span>";raisedHandCtr++;}})```

