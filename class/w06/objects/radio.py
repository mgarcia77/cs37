
class AlarmClock(object):

    def __init__(self):
        self.currentTime = 0
        self.alarmTime = 0
        self.alarmOn = False

    def status(self):
        if self.alarmOn:
            return "time %d alarm %d" % (self.currentTime, self.alarmTime)
        else:
            return "time %d alarm off" % (self.currentTime)

    def reset(self):
        self.currentTime = 0
        self.alarmTime = 0
        self.alarmOn = False
        return self.status()

    def setTime(self, newTime):
        self.currentTime = newTime
        return self.status()

    def tick(self):
        self.currentTime = self.currentTime + 1
        if self.alarmOn and (self.currentTime >= self.alarmTime):
            return "rinnnnnng %s" % (self.status())
        else:
            return self.status()

    def setAlarm(self, newTime):
        self.alarmTime = newTime
        self.alarmOn = True
        return self.status()

    def alarmOff(self):
        self.alarmOn = False
        return self.status()

class ClockRadio(AlarmClock):

    def __init__(self):
        self.band = "FM"
        self.fmFrequency = 88.5
        self.amFrequency = 1620
        super(ClockRadio, self).__init__()

    def status(self):
        freq = self.fmFrequency if self.band == "FM" else self.amFrequency
        return "band %s frequency %s %s" % (self.band, freq,
                                            super(ClockRadio, self).status())

    def changeFrequency(self, whichBand, newFrequency):
        if whichBand == "FM":
            self.band = "FM"
            self.fmFrequency = newFrequency
        else:
            self.band = "AM"
            self.amFrequency = newFrequency
        return self.status()


def main():
    print "Alarm Clock"
    myclock = AlarmClock()
    print myclock.tick()
    print myclock.tick()
    print myclock.setAlarm(4)
    print myclock.tick()
    print myclock.tick()
    print myclock.tick()
    print myclock.alarmOff()
    print myclock.tick()
    print myclock.reset()

    print
    print "Clock Radio"
    myclockradio = ClockRadio()
    print myclockradio.changeFrequency("AM", 1010)
    print myclockradio.tick()
    print myclockradio.changeFrequency("FM", 90.9)
    print myclockradio.tick()
    print myclockradio.status()
    
    

main()    
