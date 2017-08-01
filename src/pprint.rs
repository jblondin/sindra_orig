use span::Spanned;

pub trait PrettyPrint {
    fn to_pp_string(&self) -> String;
}

impl<'a, T> PrettyPrint for Spanned<'a, T> where T: PrettyPrint {
    fn to_pp_string(&self) -> String {
        self.item.to_pp_string()
    }
}

impl<A> PrettyPrint for (A,) where
        A: PrettyPrint
{
    fn to_pp_string(&self) -> String {
        format!("({})",
            self.0.to_pp_string()
        )
    }
}

impl<A, B> PrettyPrint for (A, B) where
        A: PrettyPrint,
        B: PrettyPrint
{
    fn to_pp_string(&self) -> String {
        format!("({},{})",
            self.0.to_pp_string(),
            self.1.to_pp_string()
        )
    }
}

impl<A, B, C> PrettyPrint for (A, B, C) where
        A: PrettyPrint,
        B: PrettyPrint,
        C: PrettyPrint
{
    fn to_pp_string(&self) -> String {
        format!("({},{},{})",
            self.0.to_pp_string(),
            self.1.to_pp_string(),
            self.2.to_pp_string()
        )
    }
}

impl<A, B, C, D> PrettyPrint for (A, B, C, D) where
        A: PrettyPrint,
        B: PrettyPrint,
        C: PrettyPrint,
        D: PrettyPrint
{
    fn to_pp_string(&self) -> String {
        format!("({},{},{},{})",
            self.0.to_pp_string(),
            self.1.to_pp_string(),
            self.2.to_pp_string(),
            self.3.to_pp_string()
        )
    }
}

impl<A, B, C, D, E> PrettyPrint for (A, B, C, D, E) where
        A: PrettyPrint,
        B: PrettyPrint,
        C: PrettyPrint,
        D: PrettyPrint,
        E: PrettyPrint
{
    fn to_pp_string(&self) -> String {
        format!("({},{},{},{},{})",
            self.0.to_pp_string(),
            self.1.to_pp_string(),
            self.2.to_pp_string(),
            self.3.to_pp_string(),
            self.4.to_pp_string()
        )
    }
}

impl<A, B, C, D, E, F> PrettyPrint for (A, B, C, D, E, F) where
        A: PrettyPrint,
        B: PrettyPrint,
        C: PrettyPrint,
        D: PrettyPrint,
        E: PrettyPrint,
        F: PrettyPrint
{
    fn to_pp_string(&self) -> String {
        format!("({},{},{},{},{},{})",
            self.0.to_pp_string(),
            self.1.to_pp_string(),
            self.2.to_pp_string(),
            self.3.to_pp_string(),
            self.4.to_pp_string(),
            self.5.to_pp_string()
        )
    }
}

impl<A, B, C, D, E, F, G> PrettyPrint for (A, B, C, D, E, F, G) where
        A: PrettyPrint,
        B: PrettyPrint,
        C: PrettyPrint,
        D: PrettyPrint,
        E: PrettyPrint,
        F: PrettyPrint,
        G: PrettyPrint
{
    fn to_pp_string(&self) -> String {
        format!("({},{},{},{},{},{},{})",
            self.0.to_pp_string(),
            self.1.to_pp_string(),
            self.2.to_pp_string(),
            self.3.to_pp_string(),
            self.4.to_pp_string(),
            self.5.to_pp_string(),
            self.6.to_pp_string()
        )
    }
}

impl<A, B, C, D, E, F, G, H> PrettyPrint for (A, B, C, D, E, F, G, H) where
        A: PrettyPrint,
        B: PrettyPrint,
        C: PrettyPrint,
        D: PrettyPrint,
        E: PrettyPrint,
        F: PrettyPrint,
        G: PrettyPrint,
        H: PrettyPrint
{
    fn to_pp_string(&self) -> String {
        format!("({},{},{},{},{},{},{},{})",
            self.0.to_pp_string(),
            self.1.to_pp_string(),
            self.2.to_pp_string(),
            self.3.to_pp_string(),
            self.4.to_pp_string(),
            self.5.to_pp_string(),
            self.6.to_pp_string(),
            self.7.to_pp_string()
        )
    }
}

impl<A, B, C, D, E, F, G, H, I> PrettyPrint for (A, B, C, D, E, F, G, H, I) where
        A: PrettyPrint,
        B: PrettyPrint,
        C: PrettyPrint,
        D: PrettyPrint,
        E: PrettyPrint,
        F: PrettyPrint,
        G: PrettyPrint,
        H: PrettyPrint,
        I: PrettyPrint
{
    fn to_pp_string(&self) -> String {
        format!("({},{},{},{},{},{},{},{},{})",
            self.0.to_pp_string(),
            self.1.to_pp_string(),
            self.2.to_pp_string(),
            self.3.to_pp_string(),
            self.4.to_pp_string(),
            self.5.to_pp_string(),
            self.6.to_pp_string(),
            self.7.to_pp_string(),
            self.8.to_pp_string()
        )
    }
}

impl<A, B, C, D, E, F, G, H, I, J> PrettyPrint for (A, B, C, D, E, F, G, H, I, J) where
        A: PrettyPrint,
        B: PrettyPrint,
        C: PrettyPrint,
        D: PrettyPrint,
        E: PrettyPrint,
        F: PrettyPrint,
        G: PrettyPrint,
        H: PrettyPrint,
        I: PrettyPrint,
        J: PrettyPrint
{
    fn to_pp_string(&self) -> String {
        format!("({},{},{},{},{},{},{},{},{},{})",
            self.0.to_pp_string(),
            self.1.to_pp_string(),
            self.2.to_pp_string(),
            self.3.to_pp_string(),
            self.4.to_pp_string(),
            self.5.to_pp_string(),
            self.6.to_pp_string(),
            self.7.to_pp_string(),
            self.8.to_pp_string(),
            self.9.to_pp_string()
        )
    }
}

impl<A, B, C, D, E, F, G, H, I, J, K> PrettyPrint for (A, B, C, D, E, F, G, H, I, J, K) where
        A: PrettyPrint,
        B: PrettyPrint,
        C: PrettyPrint,
        D: PrettyPrint,
        E: PrettyPrint,
        F: PrettyPrint,
        G: PrettyPrint,
        H: PrettyPrint,
        I: PrettyPrint,
        J: PrettyPrint,
        K: PrettyPrint
{
    fn to_pp_string(&self) -> String {
        format!("({},{},{},{},{},{},{},{},{},{},{})",
            self.0.to_pp_string(),
            self.1.to_pp_string(),
            self.2.to_pp_string(),
            self.3.to_pp_string(),
            self.4.to_pp_string(),
            self.5.to_pp_string(),
            self.6.to_pp_string(),
            self.7.to_pp_string(),
            self.8.to_pp_string(),
            self.9.to_pp_string(),
            self.10.to_pp_string()
        )
    }
}

impl<A, B, C, D, E, F, G, H, I, J, K, L> PrettyPrint for (A, B, C, D, E, F, G, H, I, J, K, L) where
        A: PrettyPrint,
        B: PrettyPrint,
        C: PrettyPrint,
        D: PrettyPrint,
        E: PrettyPrint,
        F: PrettyPrint,
        G: PrettyPrint,
        H: PrettyPrint,
        I: PrettyPrint,
        J: PrettyPrint,
        K: PrettyPrint,
        L: PrettyPrint
{
    fn to_pp_string(&self) -> String {
        format!("({},{},{},{},{},{},{},{},{},{},{},{})",
            self.0.to_pp_string(),
            self.1.to_pp_string(),
            self.2.to_pp_string(),
            self.3.to_pp_string(),
            self.4.to_pp_string(),
            self.5.to_pp_string(),
            self.6.to_pp_string(),
            self.7.to_pp_string(),
            self.8.to_pp_string(),
            self.9.to_pp_string(),
            self.10.to_pp_string(),
            self.11.to_pp_string()
        )
    }
}

impl<'a, T> PrettyPrint for Vec<Spanned<'a, T>> where T: PrettyPrint {
    fn to_pp_string(&self) -> String {
        let mut res = String::new();
        let mut first = true;
        for sp_item in self {
            if first { first = false; } else { res += "\n"; }
            res += &sp_item.to_pp_string();
        }
        res
    }
}
