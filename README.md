# org-clock-agenda-daytime-mode

Emacs mode to display the time clocked today in the modeline.

See
[org-clock-agenda-daytime-mode.el](org-clock-agenda-daytime-mode.el)
for usage instructions (as is common for emacs packages).

## Screenshots

If you clocked 32 minutes in total for today in the LOGBOOK drawer:

![clocked time](clocktable-entry.png)

You see that time shown in the modeline:

![modeline entry](modeline-entry.png)

When you reached `org-clock-agenda-daytime-target-work-time-minutes`
the entry changes the color to the `org-done` face (customizable):

![modeline entry, target time reached](modeline-entry-target-reached.png)

When you reach `org-clock-agenda-daytime-maximum-work-time-minutes`
the entry display changes to the `org-mode-line-clock-overrun` face (customizable):

![modeline entry, maximum time reached](modeline-entry-maximum.png)
