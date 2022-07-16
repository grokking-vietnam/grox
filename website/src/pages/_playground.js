import React from 'react'
import Layout from '@theme/Layout'
import {
    TyrianApp
} from '../../../web/target/scala-3.1.3/web-fastopt.js'


export default function Hello() {
    React.useEffect(() => { TyrianApp.launch("myapp"); }, [])
    return (
        <div
            id="myapp" />
    );
}