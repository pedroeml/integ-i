
public enum Color {
	BACK {
		@Override
		public String toString() {
			return "BACK";
		}
	}, ANORMAL {
		@Override
		public String toString() {
			return "ANORMAL";
		}
	}, TEMPORARY {
		@Override
		public String toString() {
			return "TEMPORARY";
		}
	}, PATH {
		@Override
		public String toString() {
			return "PATH";
		}
	};
}
