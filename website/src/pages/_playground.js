import React from 'react'
import Layout from '@theme/Layout'
import {
    TyrianApp
} from '../../../web/target/scala-3/main.js'


export default function Hello() {
    React.useEffect(() => { TyrianApp.launch("myapp"); }, [])
    return (
        <div
            id="myapp" />
    );
}