#  Problem: a shiny component can be interacted by the user,
#  or programmatically.
#  You want to manage the consequences separately.
#  So the function that does an update should disable some observeEvent objects.
#  When
'
When you create an observer that does an update,
register the other observers that should be suspended until done,
and then resume.
For extra credit:
  Create an array of components. (They could have multiple sub-components.)
  Create an array of observers of an array of components.
Goal: an object to hold an ObserveEvent ,
register which other observers need to be suspended, then resumed.
'

setRefClass(Class = 'EventObserver',
            fields = c('myName',
                       'reactiveDependency',
                       'observeEventExpression',
                       'observersToSuspend'),
            methods=list(
              'addObserverToSuspend'=function()1,
              'addObserverClassToSuspend'=function()2,
              'doAction'=function(){
                for(obs in observersToSuspend)
                  obs$suspend()

              }))
x1=new('EventObserver')
x1$myName = "testme"

#x1$observeEventExpression =
