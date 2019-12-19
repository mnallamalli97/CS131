import java.util.concurrent.locks.*;

class AcmeSafe implements State{
	private byte maxval;
	private byte[] value;

	private ReentrantLock lockk = new ReentrantLock();

	public int size() { return value.length; }
    public byte[] current() { return value; }

	//constructor
	AcmeSafe(byte[] v){
		value = v;
		maxval = 127;
	}

	//constructor
	AcmeSafe(byte[] v, byte maxva){
		value = v;
		maxval = maxva;
	}

	public boolean swap(int i, int j) {
		lockk.lock();

		if (value[i] <= 0 || value[j] >= maxval) {
			lockk.unlock();
		    return false;
		}

		value[i]--;
		value[j]++;
		lockk.unlock();
		return true;
	}
}